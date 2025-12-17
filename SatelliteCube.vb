
Imports System.Collections.Concurrent
Imports System.Numerics
Imports System.Threading
Imports Current.PluginApi

<PluginMetadata("Satellite Cubes Plugin", "1.2", "nasuno", "Animated rotating 3D cube demo for plugin host, with randomly colored panel lines and circles. Now supports multiple orbiting satellite cubes.")>
Public Class RotatingCubePlugin
    Implements IPlugin

    ' --- Main Orbit/Rotation Configurables ---
    ' The 3D center point about which the main cube orbits.
    ' Typical range: Any Vector3 position in world units.
    'Private orbitCenter As Vector3 = New Vector3(-250, 155, -78)

    ' --- Main Orbit/Rotation Configurables ---
    ' The 3D center point about which the main cube orbits.
    Private orbitCenter As Vector3

    ' The radius of the main cube's orbit around orbitCenter.
    ' Range: >0, in world units. Larger values mean a wider orbit.
    Private orbitRadius As Double = 1500

    ' The angular speed of the main cube's orbit (radians/second).
    ' Range: >0. Lower values = slower orbit, higher = faster.
    Private orbitSpeed As Double = Math.PI / 45

    ' The axis in 3D space about which the orbit plane tilts.
    ' Range: Any Vector3 (typically unit length).
    Private orbitTiltBaseAxis As New Vector3(1, 0, 0)

    ' The speed of orbit plane tilt (radians/second).
    ' Range: >0. Higher values = faster tilt oscillation.
    Public orbitTiltSpeed As Double = (Math.PI / 6) '* 0.8

    ' The maximum tilt angle for the orbit plane (radians).
    ' Range: 0 to PI. 0 = no tilt, PI = fully flips tilt.
    Public orbitTiltMaxAngle As Double = Math.PI

    ' The "up" axis for the main cube's orbit plane.
    ' Range: Any Vector3 (typically (0,1,0) for world Y-up).
    Private baseOrbitAxis As New Vector3(0, 1, 0)

    ' The initial phase angle of the orbit (radians).
    ' Range: 0 to 2*PI. Controls starting position in orbit.
    Private orbitPhase As Double = 0

    ' Primary axis for the cube's own rotation ("spin").
    ' Range: Any Vector3 (typically (0,1,0) for Y-up).
    Private primaryAxis As New Vector3(0, 1, 0)

    ' Speed of rotation around the primary axis (radians/second).
    ' Range: >0. Higher values = faster self-spin.
    Private primarySpeed As Double = 4 * Math.PI * 2 / 60.0

    ' Secondary axis for the cube's own rotation.
    ' Range: Any Vector3 (typically (1,0,0) for X).
    Private secondaryAxis As New Vector3(1, 0, 0)

    ' Speed of rotation around the secondary axis (radians/second).
    ' Range: >0. Typically less than primarySpeed.
    Private secondarySpeed As Double = (4 * Math.PI * 2 / 60.0) / 2

    ' Number of satellite cubes orbiting the main cube.
    ' Range: 0 or more. 0 = no satellites. Integer only.
    Public satelliteCount As Integer = 5

    ' Fractional size of each satellite cube relative to main cube.
    ' Range: 0 < satelliteSizeRatio < 1. E.g. 1/3 for small satellites.
    Private satelliteSizeRatio As Double = 1.0 / 2

    ' Multiplier for satellite orbit radius (relative to their own side length).
    ' Range: >0. Higher = satellites orbit farther from main.
    Private satelliteDistanceRatio As Double = 4.5

    Private Const CubeStructBase As Integer = 55555
    Private cubes As List(Of CubeInstance)
    Private creationTime As DateTime
    Private animationThread As Thread
    Private animationShouldRun As Boolean = False

    ' Current positions
    Private cubePositions As New Dictionary(Of Integer, Vector3)
    Private cubeRotations As New Dictionary(Of Integer, Quaternion)

    ' Collision immunity tracking
    Private collisionImmunityTime As Double = 1.7 '0.3  ' 1 second immunity after collision
    Private collisionImmunity As New Dictionary(Of String, Double)  ' Key = "cube1Index_cube2Index", Value = timeWhenImmunityExpires

    Public Sub StopAnimation()
        animationShouldRun = False
        If animationThread IsNot Nothing AndAlso animationThread.IsAlive Then
            animationThread.Join(500)
        End If
    End Sub

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute

        Dim leftCol As Integer = api.GetPanelFurthestLeftColumn(PanelType.WestPanel)
        Dim rightCol As Integer = api.GetPanelFurthestRightColumn(PanelType.EastPanel)
        Dim topRow As Integer = api.GetPanelFurthestTopRow(PanelType.TopPanel)
        Dim bottomRow As Integer = api.GetPanelFurthestBottomRow(PanelType.BottomPanel)
        Dim frontCol As Integer = api.GetPanelFurthestLeftColumn(PanelType.NorthPanel)
        Dim backCol As Integer = api.GetPanelFurthestRightColumn(PanelType.SouthPanel)

        Dim centerX As Integer = (leftCol + rightCol) \ 2
        Dim centerY As Integer = (topRow + bottomRow) \ 2
        Dim centerZ As Integer = (frontCol + backCol) \ 2

        ' Assign orbit center dynamically
        orbitCenter = New Vector3(centerX, centerY, centerZ)


        If animationThread IsNot Nothing AndAlso animationThread.IsAlive Then Return
        animationShouldRun = True
        animationThread = New Thread(Sub() AnimationLoop(api))
        animationThread.IsBackground = True
        animationThread.Start()
    End Sub

    ' Rotates cubeA in place to minimize overlap with cubeB.
    Private Sub FindMinimalOverlapRotation(
        ByRef cubeA As CubeInstance,
        ByVal posA As Vector3,
        ByVal sizeA As Double,
        ByVal origRotA As Quaternion,
        ByVal cubeB As CubeInstance,
        ByVal posB As Vector3,
        ByVal sizeB As Double,
        ByVal rotB As Quaternion,
        Optional ByVal angleStepDegrees As Integer = 10,
        Optional ByVal maxAngleDegrees As Integer = 40
    )
        Dim minOverlapCount As Integer = Integer.MaxValue
        Dim bestRot As Quaternion = origRotA

        For dx As Integer = -maxAngleDegrees To maxAngleDegrees Step angleStepDegrees
            For dy As Integer = -maxAngleDegrees To maxAngleDegrees Step angleStepDegrees
                For dz As Integer = -maxAngleDegrees To maxAngleDegrees Step angleStepDegrees
                    Dim qx = Quaternion.CreateFromAxisAngle(New Vector3(1, 0, 0), CSng(Math.PI * dx / 180.0))
                    Dim qy = Quaternion.CreateFromAxisAngle(New Vector3(0, 1, 0), CSng(Math.PI * dy / 180.0))
                    Dim qz = Quaternion.CreateFromAxisAngle(New Vector3(0, 0, 1), CSng(Math.PI * dz / 180.0))
                    Dim candidateRot = Quaternion.Normalize(Quaternion.Multiply(Quaternion.Multiply(qx, qy), qz))
                    candidateRot = Quaternion.Normalize(Quaternion.Multiply(origRotA, candidateRot))

                    Dim boundsA = GetCubeBounds(posA, sizeA, candidateRot)
                    Dim boundsB = GetCubeBounds(posB, sizeB, rotB)

                    Dim overlapCount = GetOverlapCount(boundsA, boundsB)
                    If overlapCount < minOverlapCount Then
                        minOverlapCount = overlapCount
                        bestRot = candidateRot
                    End If

                    If overlapCount = 0 Then
                        cubeA.currentQuaternion = bestRot
                        Exit Sub
                    End If
                Next
            Next
        Next

        cubeA.currentQuaternion = bestRot
    End Sub

    ' Helper: Count overlapping corners (simple but effective for cubes)
    Private Function GetOverlapCount(boundsA() As Vector3, boundsB() As Vector3) As Integer
        Dim overlap As Integer = 0
        For Each pt In boundsA
            If PointInsideCube(pt, boundsB) Then overlap += 1
        Next
        For Each pt In boundsB
            If PointInsideCube(pt, boundsA) Then overlap += 1
        Next
        Return overlap
    End Function

    ' Helper: Point-in-cube by bounding box (fast, works for cubes)
    Private Function PointInsideCube(pt As Vector3, cubeCorners() As Vector3) As Boolean
        Dim minX = cubeCorners.Min(Function(v) v.X)
        Dim maxX = cubeCorners.Max(Function(v) v.X)
        Dim minY = cubeCorners.Min(Function(v) v.Y)
        Dim maxY = cubeCorners.Max(Function(v) v.Y)
        Dim minZ = cubeCorners.Min(Function(v) v.Z)
        Dim maxZ = cubeCorners.Max(Function(v) v.Z)
        Return (pt.X >= minX AndAlso pt.X <= maxX) AndAlso
           (pt.Y >= minY AndAlso pt.Y <= maxY) AndAlso
           (pt.Z >= minZ AndAlso pt.Z <= maxZ)
    End Function

    ' Minimize overlap between two cubes by rotating each in place.
    Private Sub MinimizeOverlapOnCollision(cubeA As CubeInstance, cubeB As CubeInstance)
        FindMinimalOverlapRotation(
            cubeA, cubeA.cubeCenter, cubeA.Side, cubeA.currentQuaternion,
            cubeB, cubeB.cubeCenter, cubeB.Side, cubeB.currentQuaternion
        )
        FindMinimalOverlapRotation(
            cubeB, cubeB.cubeCenter, cubeB.Side, cubeB.currentQuaternion,
            cubeA, cubeA.cubeCenter, cubeA.Side, cubeA.currentQuaternion
        )
    End Sub

    ' Full-CPU separation loop: repeatedly minimize overlap and draw after each step until all cubes are separated.
    Private Sub RunSeparationLoop(api As ICurrentApi)
        Dim stillColliding As Boolean = True
        Dim maxSeparationSteps As Integer = 1000 ' Avoid infinite loop
        Dim stepCount As Integer = 0

        Do While stillColliding AndAlso stepCount < maxSeparationSteps
            stepCount += 1
            stillColliding = False
            Dim now As Double = (DateTime.Now - creationTime).TotalSeconds

            For i As Integer = 1 To cubes.Count - 1
                For j As Integer = i + 1 To cubes.Count - 1
                    If HasCollisionImmunity(i, j, now) Then Continue For

                    If CheckCubeCollision(cubePositions(i), cubes(i).Side, cubes(i).currentQuaternion,
                                     cubePositions(j), cubes(j).Side, cubes(j).currentQuaternion) Then
                        MinimizeOverlapOnCollision(cubes(i), cubes(j))
                        SetCollisionImmunity(i, j, now)
                        stillColliding = True
                    End If
                Next
            Next

            ' Draw all cubes after each step
            For k As Integer = 0 To cubes.Count - 1
                If k = 0 Then
                    cubes(k).UpdateWithPositionAndRotation(0, Nothing, cubePositions(k), cubes(k).currentQuaternion, api)
                Else
                    cubes(k).UpdateWithPositionAndRotation(0, cubes(0), cubePositions(k), cubes(k).currentQuaternion, api)
                End If
            Next
        Loop
    End Sub

    Private Sub AnimationLoop(api As ICurrentApi)
        creationTime = DateTime.Now
        Dim rand As New Random()
        cubes = New List(Of CubeInstance)

        ' Main Cube (Parent)
        cubes.Add(New CubeInstance(
            isSatellite:=False,
            structBase:=CubeStructBase,
            side:=400,
            orbitCenter:=orbitCenter,
            orbitRadius:=orbitRadius,
            orbitSpeed:=orbitSpeed,
            orbitTiltBaseAxis:=orbitTiltBaseAxis,
            orbitTiltSpeed:=orbitTiltSpeed,
            orbitTiltMaxAngle:=orbitTiltMaxAngle,
            baseOrbitAxis:=baseOrbitAxis,
            orbitPhase:=orbitPhase,
            primaryAxis:=primaryAxis,
            primarySpeed:=primarySpeed,
            secondaryAxis:=secondaryAxis,
            secondarySpeed:=secondarySpeed,
            satelliteIndex:=0,
            rand:=rand
        ))

        ' Satellites
        For satIdx As Integer = 0 To satelliteCount - 1
            Dim randomTiltBaseAxis As Vector3 = RandomUnitVector(rand)
            Dim randomBaseOrbitAxis As Vector3 = Vector3.Normalize(RandomUnitVector(rand))
            Dim randomPhase As Double = rand.NextDouble() * 2 * Math.PI
            Dim randomPrimaryAxis As Vector3 = Vector3.Normalize(RandomUnitVector(rand))
            Dim randomSecondaryAxis As Vector3
            Do
                randomSecondaryAxis = Vector3.Normalize(RandomUnitVector(rand))
            Loop While Math.Abs(Vector3.Dot(randomSecondaryAxis, randomPrimaryAxis)) > 0.95
            cubes.Add(New CubeInstance(
                isSatellite:=True,
                structBase:=CubeStructBase + 1000 * (satIdx + 1),
                side:=400 * satelliteSizeRatio,
                orbitCenter:=Vector3.Zero,
                orbitRadius:=400 * satelliteSizeRatio * satelliteDistanceRatio,
                orbitSpeed:=orbitSpeed * 1.8 + rand.NextDouble() * 0.2,
                orbitTiltBaseAxis:=randomTiltBaseAxis,
                orbitTiltSpeed:=orbitTiltSpeed * (0.8 + 0.4 * rand.NextDouble()),
                orbitTiltMaxAngle:=orbitTiltMaxAngle,
                baseOrbitAxis:=randomBaseOrbitAxis,
                orbitPhase:=randomPhase,
                primaryAxis:=randomPrimaryAxis,
                primarySpeed:=primarySpeed * 1.3,
                secondaryAxis:=randomSecondaryAxis,
                secondarySpeed:=secondarySpeed * 1.1,
                satelliteIndex:=satIdx + 1,
                rand:=rand
            ))
        Next

        For Each cube In cubes
            api.SetStructureDrawState(cube.StructureIds.Edges, True)
            api.SetStructureDrawState(cube.StructureIds.Highlight, True)
            api.SetStructureDrawState(cube.StructureIds.Circles, True)
        Next

        ' First frame initialization
        Dim elapsedSeconds As Double = 0

        ' Initialize main cube
        Dim mainCubePos = CalculateNextPosition(cubes(0), elapsedSeconds, Vector3.Zero)
        Dim mainCubeRot = CalculateNextRotation(cubes(0), elapsedSeconds)
        cubePositions(0) = mainCubePos
        cubeRotations(0) = mainCubeRot
        cubes(0).UpdateWithPositionAndRotation(elapsedSeconds, Nothing, mainCubePos, mainCubeRot, api)

        ' Initialize satellites
        For i As Integer = 1 To cubes.Count - 1
            Dim pos = CalculateNextPosition(cubes(i), elapsedSeconds, mainCubePos)
            Dim rot = CalculateNextRotation(cubes(i), elapsedSeconds)
            cubePositions(i) = pos
            cubeRotations(i) = rot
            cubes(i).UpdateWithPositionAndRotation(elapsedSeconds, cubes(0), pos, rot, api)
        Next
















        Console.WriteLine(api.objectDictionary.Count)
        ' === Thin scene after all objects constructed ===
        api.ThinEvenSpatiallyAdaptiveAuto(api.objectDictionary, Nothing, 7500, api.GetObserverOrigin(), 200, 50, 0.2)
        Console.WriteLine(api.objectDictionary.Count)

        ' === Refresh each cube's ID lists to match surviving objects ===
        For Each cube In cubes
            cube.RefreshObjectIdsAfterThinning(api)
        Next



















        Do While animationShouldRun
            Try
                Dim hadCollision = RotatingCubesFrame(api)

                If hadCollision Then
                    RunSeparationLoop(api)
                Else
                    Thread.Sleep(40)
                End If
            Catch ex As Exception
            End Try
        Loop

        For Each cube In cubes
            api.RemoveObjectsByStructureId(cube.StructureIds.Edges)
            api.RemoveObjectsByStructureId(cube.StructureIds.Highlight)
            api.RemoveObjectsByStructureId(cube.StructureIds.Circles)
            api.SetStructureDrawState(cube.StructureIds.Edges, False)
            api.SetStructureDrawState(cube.StructureIds.Highlight, False)
            api.SetStructureDrawState(cube.StructureIds.Circles, False)
        Next
    End Sub

    ' Create a unique key for the collision immunity dictionary
    Private Function MakeCollisionKey(cubeA As Integer, cubeB As Integer) As String
        If cubeA < cubeB Then
            Return cubeA & "_" & cubeB
        Else
            Return cubeB & "_" & cubeA
        End If
    End Function

    ' Check if two cubes have collision immunity
    Private Function HasCollisionImmunity(cubeA As Integer, cubeB As Integer, currentTime As Double) As Boolean
        Dim key As String = MakeCollisionKey(cubeA, cubeB)

        If collisionImmunity.ContainsKey(key) Then
            Dim expiryTime As Double = collisionImmunity(key)
            If currentTime > expiryTime Then
                collisionImmunity.Remove(key)
                Return False
            End If
            Return True
        End If

        Return False
    End Function

    ' Set collision immunity between two cubes
    Private Sub SetCollisionImmunity(cubeA As Integer, cubeB As Integer, currentTime As Double)
        Dim key As String = MakeCollisionKey(cubeA, cubeB)
        collisionImmunity(key) = currentTime + collisionImmunityTime
    End Sub

    ' Only check satellites against each other (host is excluded)
    Private Function RotatingCubesFrame(api As ICurrentApi) As Boolean
        Dim now As DateTime = DateTime.Now
        Dim elapsedSeconds As Double = (now - creationTime).TotalSeconds

        ' Calculate main cube's next position and rotation
        Dim mainCubeNextPos = CalculateNextPosition(cubes(0), elapsedSeconds, Vector3.Zero)
        Dim mainCubeNextRot = CalculateNextRotation(cubes(0), elapsedSeconds)

        ' Calculate satellite next positions and rotations
        Dim satelliteNextPositions As New Dictionary(Of Integer, Vector3)
        Dim satelliteNextRotations As New Dictionary(Of Integer, Quaternion)

        For i As Integer = 1 To cubes.Count - 1
            satelliteNextPositions(i) = CalculateNextPosition(cubes(i), elapsedSeconds, mainCubeNextPos)
            satelliteNextRotations(i) = CalculateNextRotation(cubes(i), elapsedSeconds)
        Next

        ' Check for collisions and apply direction changes
        Dim collisions As New List(Of Tuple(Of Integer, Integer))

        ' Only check satellites against each other (no host-to-satellite checks)
        For i As Integer = 1 To cubes.Count - 1
            For j As Integer = i + 1 To cubes.Count - 1
                If HasCollisionImmunity(i, j, elapsedSeconds) Then Continue For

                If CheckCubeCollision(satelliteNextPositions(i), cubes(i).Side, satelliteNextRotations(i),
                                    satelliteNextPositions(j), cubes(j).Side, satelliteNextRotations(j)) Then
                    collisions.Add(New Tuple(Of Integer, Integer)(i, j))
                    SetCollisionImmunity(i, j, elapsedSeconds)
                End If
            Next
        Next

        If collisions.Count > 0 Then
            For Each pair In collisions
                Dim cubeA = pair.Item1
                Dim cubeB = pair.Item2
                ReverseDirection(cubes(cubeA), elapsedSeconds)
                ReverseDirection(cubes(cubeB), elapsedSeconds)
            Next

            cubes(0).UpdateWithPositionAndRotation(elapsedSeconds, Nothing, cubePositions(0), cubeRotations(0), api)
            For i As Integer = 1 To cubes.Count - 1
                cubes(i).UpdateWithPositionAndRotation(elapsedSeconds, cubes(0), cubePositions(i), cubeRotations(i), api)
            Next

            Return True
        Else
            cubePositions(0) = mainCubeNextPos
            cubeRotations(0) = mainCubeNextRot
            cubes(0).UpdateWithPositionAndRotation(elapsedSeconds, Nothing, mainCubeNextPos, mainCubeNextRot, api)
            For i As Integer = 1 To cubes.Count - 1
                cubePositions(i) = satelliteNextPositions(i)
                cubeRotations(i) = satelliteNextRotations(i)
                cubes(i).UpdateWithPositionAndRotation(elapsedSeconds, cubes(0), satelliteNextPositions(i), satelliteNextRotations(i), api)
            Next

            Return False
        End If
    End Function

    Private Sub ReverseDirection(cube As CubeInstance, elapsedSeconds As Double)
        Dim currentAngle As Double = cube.orbitPhase + cube.orbitSpeed * elapsedSeconds
        cube.orbitSpeed *= -1
        cube.orbitPhase = currentAngle - cube.orbitSpeed * elapsedSeconds
    End Sub

    Private Function CheckCubeCollision(pos1 As Vector3, size1 As Double, rot1 As Quaternion,
                                       pos2 As Vector3, size2 As Double, rot2 As Quaternion) As Boolean
        Dim bounds1 = GetCubeBounds(pos1, size1, rot1)
        Dim bounds2 = GetCubeBounds(pos2, size2, rot2)
        Return CubesIntersect(bounds1, bounds2)
    End Function

    Private Function CalculateNextPosition(cube As CubeInstance, elapsedSeconds As Double, parentCenter As Vector3) As Vector3
        Dim tiltAngle As Double = (cube.orbitTiltSpeed * elapsedSeconds) Mod (2 * Math.PI)
        If cube.orbitTiltMaxAngle < Math.PI Then tiltAngle = Math.Min(tiltAngle, cube.orbitTiltMaxAngle)

        Dim tiltQuat As Quaternion = Quaternion.CreateFromAxisAngle(
            Vector3.Normalize(cube.orbitTiltBaseAxis), CSng(tiltAngle))

        Dim right As Vector3 = Vector3.Transform(cube.localRight, tiltQuat)
        Dim forward As Vector3 = Vector3.Transform(cube.localForward, tiltQuat)

        Dim orbitAngle As Double = cube.orbitPhase + cube.orbitSpeed * elapsedSeconds
        Dim orbitOffset As Vector3 = CSng(cube.orbitRadius * Math.Cos(orbitAngle)) * right +
                                    CSng(cube.orbitRadius * Math.Sin(orbitAngle)) * forward

        If cube.IsSatellite Then
            Return parentCenter + orbitOffset
        Else
            Return cube.orbitCenter + orbitOffset
        End If
    End Function

    Private Function CalculateNextRotation(cube As CubeInstance, elapsedSeconds As Double) As Quaternion
        Dim qPrimary = Quaternion.CreateFromAxisAngle(Vector3.Normalize(cube.primaryAxis), CSng(cube.primarySpeed * elapsedSeconds))
        Dim qSecondary = Quaternion.CreateFromAxisAngle(Vector3.Normalize(cube.secondaryAxis), CSng(cube.secondarySpeed * elapsedSeconds))
        Return Quaternion.Normalize(Quaternion.Multiply(qPrimary, qSecondary))
    End Function

    Private Function GetCubeBounds(center As Vector3, size As Double, rotation As Quaternion) As Vector3()
        Dim half As Single = CSng(size / 2)
        Dim corners(7) As Vector3

        corners(0) = New Vector3(-half, -half, -half)
        corners(1) = New Vector3(half, -half, -half)
        corners(2) = New Vector3(half, -half, half)
        corners(3) = New Vector3(-half, -half, half)
        corners(4) = New Vector3(-half, half, -half)
        corners(5) = New Vector3(half, half, -half)
        corners(6) = New Vector3(half, half, half)
        corners(7) = New Vector3(-half, half, half)

        For i As Integer = 0 To 7
            corners(i) = Vector3.Transform(corners(i), rotation) + center
        Next

        Return corners
    End Function

    Private Function CubesIntersect(cornersA() As Vector3, cornersB() As Vector3) As Boolean
        Dim normalsA(2) As Vector3
        normalsA(0) = Vector3.Normalize(cornersA(1) - cornersA(0))
        normalsA(1) = Vector3.Normalize(cornersA(4) - cornersA(0))
        normalsA(2) = Vector3.Normalize(cornersA(3) - cornersA(0))

        Dim normalsB(2) As Vector3
        normalsB(0) = Vector3.Normalize(cornersB(1) - cornersB(0))
        normalsB(1) = Vector3.Normalize(cornersB(4) - cornersB(0))
        normalsB(2) = Vector3.Normalize(cornersB(3) - cornersB(0))

        For Each normal In normalsA
            If NotOverlappingInDirection(cornersA, cornersB, normal) Then
                Return False
            End If
        Next

        For Each normal In normalsB
            If NotOverlappingInDirection(cornersA, cornersB, normal) Then
                Return False
            End If
        Next

        For Each normalA In normalsA
            For Each normalB In normalsB
                Dim crossNormal = Vector3.Cross(normalA, normalB)
                If crossNormal.LengthSquared() > 0.001F Then
                    crossNormal = Vector3.Normalize(crossNormal)
                    If NotOverlappingInDirection(cornersA, cornersB, crossNormal) Then
                        Return False
                    End If
                End If
            Next
        Next

        Return True
    End Function

    Private Function NotOverlappingInDirection(cornersA() As Vector3, cornersB() As Vector3, direction As Vector3) As Boolean
        Dim minA As Single = Single.MaxValue
        Dim maxA As Single = Single.MinValue
        For Each corner In cornersA
            Dim projection = Vector3.Dot(corner, direction)
            minA = Math.Min(minA, projection)
            maxA = Math.Max(maxA, projection)
        Next

        Dim minB As Single = Single.MaxValue
        Dim maxB As Single = Single.MinValue
        For Each corner In cornersB
            Dim projection = Vector3.Dot(corner, direction)
            minB = Math.Min(minB, projection)
            maxB = Math.Max(maxB, projection)
        Next

        Return (maxA < minB) OrElse (maxB < minA)
    End Function


















    Private Class CubeInstance
        Public Structure StructureIdSet
            Public Edges As Integer
            Public Highlight As Integer
            Public Circles As Integer
        End Structure

        Public Property IsSatellite As Boolean
        Public Property Side As Double
        Public Property StructureIds As StructureIdSet

        Public cubeCenter As Vector3
        Public orbitSpeed As Double
        Public orbitCenter As Vector3
        Public orbitRadius As Double
        Public orbitTiltBaseAxis As Vector3
        Public orbitTiltSpeed As Double
        Public orbitTiltMaxAngle As Double
        Public baseOrbitAxis As Vector3
        Public orbitPhase As Double
        Public primaryAxis As Vector3
        Public primarySpeed As Double
        Public secondaryAxis As Vector3
        Public secondarySpeed As Double
        Public satelliteIndex As Integer

        Public ReadOnly localRight As Vector3 = New Vector3(1, 0, 0)
        Public ReadOnly localForward As Vector3 = New Vector3(0, 0, 1)

        Public currentQuaternion As Quaternion = Quaternion.Identity
        Private initialized As Boolean = False

        Private cubeObjectIds As New List(Of Integer)
        Private cubeLocalOffsets As New List(Of Vector3)
        Private highlightObjectIds As New List(Of Integer)
        Private highlightLocalOffsets As New List(Of Vector3)
        Private highlightColors As New List(Of ObjectColor)
        Private circlesObjectIds As New List(Of Integer)
        Private circlesLocalOffsets As New List(Of Vector3)
        Private circlesColors As New List(Of ObjectColor)

        Private rand As Random

        Public Sub New(
            isSatellite As Boolean,
            structBase As Integer,
            side As Double,
            orbitCenter As Vector3,
            orbitRadius As Double,
            orbitSpeed As Double,
            orbitTiltBaseAxis As Vector3,
            orbitTiltSpeed As Double,
            orbitTiltMaxAngle As Double,
            baseOrbitAxis As Vector3,
            orbitPhase As Double,
            primaryAxis As Vector3,
            primarySpeed As Double,
            secondaryAxis As Vector3,
            secondarySpeed As Double,
            satelliteIndex As Integer,
            rand As Random
        )
            Me.IsSatellite = isSatellite
            Me.Side = side
            Me.StructureIds = New StructureIdSet With {
                .Edges = structBase,
                .Highlight = structBase + 1,
                .Circles = structBase + 2
            }
            Me.orbitCenter = orbitCenter
            Me.orbitRadius = orbitRadius
            Me.orbitSpeed = orbitSpeed
            Me.orbitTiltBaseAxis = orbitTiltBaseAxis
            Me.orbitTiltSpeed = orbitTiltSpeed
            Me.orbitTiltMaxAngle = orbitTiltMaxAngle
            Me.baseOrbitAxis = baseOrbitAxis
            Me.orbitPhase = orbitPhase
            Me.primaryAxis = primaryAxis
            Me.primarySpeed = primarySpeed
            Me.secondaryAxis = secondaryAxis
            Me.secondarySpeed = secondarySpeed
            Me.satelliteIndex = satelliteIndex
            Me.rand = rand
        End Sub

        Public Sub UpdateWithPositionAndRotation(elapsedSeconds As Double, parent As CubeInstance, position As Vector3, rotation As Quaternion, api As ICurrentApi)
            If IsSatellite AndAlso parent IsNot Nothing Then
                orbitCenter = parent.cubeCenter
            End If

            cubeCenter = position
            currentQuaternion = rotation

            If Not initialized Then
                InitializeObjects(api)
            End If

            UpdateVisuals(api)
        End Sub

        Private Sub UpdateVisuals(api As ICurrentApi)
            Dim rotate As Func(Of Vector3, Vector3) = Function(v) Vector3.Transform(v, currentQuaternion)

            For i As Integer = 0 To cubeObjectIds.Count - 1
                Dim objId As Integer = cubeObjectIds(i)
                Dim obj As MyObject = Nothing
                If DirectCast(api.objectDictionary, System.Collections.Concurrent.ConcurrentDictionary(Of Integer, MyObject)).TryGetValue(objId, obj) Then
                    Dim offset As Vector3 = cubeLocalOffsets(i)
                    Dim rotated = rotate(offset)
                    obj.UpdateLocation(
                        CLng(Math.Round(cubeCenter.X + rotated.X)),
                        CLng(Math.Round(cubeCenter.Y + rotated.Y)),
                        CLng(Math.Round(cubeCenter.Z + rotated.Z))
                    )
                End If
            Next

            For i As Integer = 0 To highlightObjectIds.Count - 1
                Dim objId As Integer = highlightObjectIds(i)
                Dim obj As MyObject = Nothing
                If DirectCast(api.objectDictionary, System.Collections.Concurrent.ConcurrentDictionary(Of Integer, MyObject)).TryGetValue(objId, obj) Then
                    Dim offset As Vector3 = highlightLocalOffsets(i)
                    Dim rotated = rotate(offset)
                    obj.UpdateLocation(
                        CLng(Math.Round(cubeCenter.X + rotated.X)),
                        CLng(Math.Round(cubeCenter.Y + rotated.Y)),
                        CLng(Math.Round(cubeCenter.Z + rotated.Z))
                    )
                End If
            Next

            For i As Integer = 0 To circlesObjectIds.Count - 1
                Dim objId As Integer = circlesObjectIds(i)
                Dim obj As MyObject = Nothing
                If DirectCast(api.objectDictionary, System.Collections.Concurrent.ConcurrentDictionary(Of Integer, MyObject)).TryGetValue(objId, obj) Then
                    Dim offset As Vector3 = circlesLocalOffsets(i)
                    Dim rotated = rotate(offset)
                    obj.UpdateLocation(
                        CLng(Math.Round(cubeCenter.X + rotated.X)),
                        CLng(Math.Round(cubeCenter.Y + rotated.Y)),
                        CLng(Math.Round(cubeCenter.Z + rotated.Z))
                    )
                End If
            Next

            UpdateCollisionTriangles(api)
        End Sub

        Private Sub UpdateCollisionTriangles(api As ICurrentApi)
            Dim side As Double = Me.Side
            Dim half As Double = side / 2
            Dim corners(7) As Vector3
            corners(0) = New Vector3(-half, -half, -half)
            corners(1) = New Vector3(half, -half, -half)
            corners(2) = New Vector3(half, -half, half)
            corners(3) = New Vector3(-half, -half, half)
            corners(4) = New Vector3(-half, half, -half)
            corners(5) = New Vector3(half, half, -half)
            corners(6) = New Vector3(half, half, half)
            corners(7) = New Vector3(-half, half, half)

            Dim rotate As Func(Of Vector3, Vector3) = Function(v) Vector3.Transform(v, currentQuaternion)

            api.RemoveAllTrianglesInSet(StructureIds.Edges)
            Dim recessedCorners(7) As Vector3
            For i As Integer = 0 To 7
                Dim v = rotate(corners(i))
                Dim dir = Vector3.Normalize(v)
                recessedCorners(i) = New Vector3(
                    CSng(cubeCenter.X + v.X - dir.X),
                    CSng(cubeCenter.Y + v.Y - dir.Y),
                    CSng(cubeCenter.Z + v.Z - dir.Z)
                )
            Next

            Dim faceIndices As Integer(,) = {
                {0, 1, 2, 3},
                {4, 5, 6, 7},
                {0, 1, 5, 4},
                {2, 3, 7, 6},
                {1, 2, 6, 5},
                {0, 3, 7, 4}
            }

            For f As Integer = 0 To 5
                Dim idx0 = faceIndices(f, 0)
                Dim idx1 = faceIndices(f, 1)
                Dim idx2 = faceIndices(f, 2)
                Dim idx3 = faceIndices(f, 3)
                api.AddTriangle(
                    recessedCorners(idx0).X, recessedCorners(idx0).Y, recessedCorners(idx0).Z,
                    recessedCorners(idx1).X, recessedCorners(idx1).Y, recessedCorners(idx1).Z,
                    recessedCorners(idx2).X, recessedCorners(idx2).Y, recessedCorners(idx2).Z,
                    StructureIds.Edges
                )
                api.AddTriangle(
                    recessedCorners(idx0).X, recessedCorners(idx0).Y, recessedCorners(idx0).Z,
                    recessedCorners(idx2).X, recessedCorners(idx2).Y, recessedCorners(idx2).Z,
                    recessedCorners(idx3).X, recessedCorners(idx3).Y, recessedCorners(idx3).Z,
                    StructureIds.Edges
                )
            Next
        End Sub

        Private Sub InitializeObjects(api As ICurrentApi)
            Dim side As Double = Me.Side
            Dim half As Double = side / 2
            Dim corners(7) As Vector3
            corners(0) = New Vector3(-half, -half, -half)
            corners(1) = New Vector3(half, -half, -half)
            corners(2) = New Vector3(half, -half, half)
            corners(3) = New Vector3(-half, -half, half)
            corners(4) = New Vector3(-half, half, -half)
            corners(5) = New Vector3(half, half, -half)
            corners(6) = New Vector3(half, half, half)
            corners(7) = New Vector3(-half, half, half)

            Dim objectDict = DirectCast(api.objectDictionary, System.Collections.Concurrent.ConcurrentDictionary(Of Integer, MyObject))
            Dim structureObjIds = DirectCast(api.structureObjectIDs, System.Collections.Concurrent.ConcurrentDictionary(Of Integer, System.Collections.Immutable.ImmutableList(Of Integer)))
            Dim rand As New Random()

            ' 1. CUBE EDGES (just like your working code)
            Dim edges As Integer(,) = {
                {0, 1}, {1, 2}, {2, 3}, {3, 0},
                {4, 5}, {5, 6}, {6, 7}, {7, 4},
                {0, 4}, {1, 5}, {2, 6}, {3, 7}
            }
            For edgeIdx As Integer = 0 To edges.GetLength(0) - 1
                Dim idxA As Integer = edges(edgeIdx, 0)
                Dim idxB As Integer = edges(edgeIdx, 1)
                Dim ptA As Vector3 = corners(idxA)
                Dim ptB As Vector3 = corners(idxB)
                Dim ax As Integer = CInt(Math.Round(cubeCenter.X + ptA.X))
                Dim ay As Integer = CInt(Math.Round(cubeCenter.Y + ptA.Y))
                Dim az As Integer = CInt(Math.Round(cubeCenter.Z + ptA.Z))
                Dim bx As Integer = CInt(Math.Round(cubeCenter.X + ptB.X))
                Dim by As Integer = CInt(Math.Round(cubeCenter.Y + ptB.Y))
                Dim bz As Integer = CInt(Math.Round(cubeCenter.Z + ptB.Z))
                Dim pts = api.Bresenham3D(ax, ay, az, bx, by, bz)
                For Each pt In pts
                    Dim lx As Single = CSng(pt.Item1 - cubeCenter.X)
                    Dim ly As Single = CSng(pt.Item2 - cubeCenter.Y)
                    Dim lz As Single = CSng(pt.Item3 - cubeCenter.Z)
                    cubeLocalOffsets.Add(New Vector3(lx, ly, lz))
                    Dim key As Integer = api.AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, StructureIds.Edges)
                    cubeObjectIds.Add(key)
                Next
            Next

            ' 2. HIGHLIGHT PANEL LINES (same as working code)
            Const highlightedPanelIndex As Integer = 1
            Dim panelEdgeIndices As Integer()() = {
                New Integer() {0, 1, 5, 4},
                New Integer() {1, 2, 6, 5},
                New Integer() {2, 3, 7, 6},
                New Integer() {3, 0, 4, 7}
            }
            Const highlightStepCount As Integer = 12
            Dim hc = panelEdgeIndices(highlightedPanelIndex)
            Dim v0 = corners(hc(0))
            Dim v1 = corners(hc(1))
            Dim v2 = corners(hc(2))
            Dim v3 = corners(hc(3))
            Dim colorChoices = GetNonWhiteColors()
            For i As Integer = 1 To highlightStepCount - 1
                Dim t As Double = i / highlightStepCount
                Dim startPt As Vector3 = Lerp3DVec(v0, v3, t)
                Dim endPt As Vector3 = Lerp3DVec(v1, v2, t)
                Dim ax As Integer = CInt(Math.Round(cubeCenter.X + startPt.X))
                Dim ay As Integer = CInt(Math.Round(cubeCenter.Y + startPt.Y))
                Dim az As Integer = CInt(Math.Round(cubeCenter.Z + startPt.Z))
                Dim bx As Integer = CInt(Math.Round(cubeCenter.X + endPt.X))
                Dim by As Integer = CInt(Math.Round(cubeCenter.Y + endPt.Y))
                Dim bz As Integer = CInt(Math.Round(cubeCenter.Z + endPt.Z))
                Dim pts = api.Bresenham3D(ax, ay, az, bx, by, bz)
                Dim lineColor As ObjectColor = colorChoices(rand.Next(colorChoices.Count))
                For Each pt In pts
                    Dim lx As Single = CSng(pt.Item1 - cubeCenter.X)
                    Dim ly As Single = CSng(pt.Item2 - cubeCenter.Y)
                    Dim lz As Single = CSng(pt.Item3 - cubeCenter.Z)
                    highlightLocalOffsets.Add(New Vector3(lx, ly, lz))
                    Dim key As Integer = api.AddMyObjectToFactory(pt.Item1, pt.Item2, pt.Item3, StructureIds.Highlight)
                    highlightObjectIds.Add(key)
                    highlightColors.Add(lineColor)
                Next
            Next
            Dim highlightObjIdsList As System.Collections.Immutable.ImmutableList(Of Integer) = Nothing
            If structureObjIds.TryGetValue(StructureIds.Highlight, highlightObjIdsList) Then
                For i As Integer = 0 To highlightObjIdsList.Count - 1
                    Dim objId = highlightObjIdsList(i)
                    Dim obj As MyObject = Nothing
                    If objectDict.TryGetValue(objId, obj) Then
                        obj.ColorOverride = highlightColors(i)
                    End If
                Next
            End If

            ' 3. CIRCLES PANEL (non-overlapping, always fits, always present)
            Dim circlesPanelIndex As Integer = 3
            Dim cc = panelEdgeIndices(circlesPanelIndex)
            Dim cv0 = corners(cc(0))
            Dim cv1 = corners(cc(1))
            Dim cv2 = corners(cc(2))
            Dim cv3 = corners(cc(3))
            Const circleRows As Integer = 4
            Const circleCols As Integer = 4
            Const minRadius As Double = 30
            Const maxRadius As Double = 60
            'Dim circlePoints As Integer = Math.Max(12, CInt(Math.Round(2 * Math.PI * radius)))
            Const epsilon As Double = 1.0 ' safety margin

            ' Precompute all grid centers
            Dim gridCenters(circleRows - 1, circleCols - 1) As Vector3
            For row As Integer = 0 To circleRows - 1
                For col As Integer = 0 To circleCols - 1
                    Dim u As Double = (col + 0.5) / circleCols
                    Dim v As Double = (row + 0.5) / circleRows
                    Dim leftEdge As Vector3 = Lerp3DVec(cv0, cv3, v)
                    Dim rightEdge As Vector3 = Lerp3DVec(cv1, cv2, v)
                    gridCenters(row, col) = Lerp3DVec(leftEdge, rightEdge, u)
                Next
            Next

            Dim circleRadii(circleRows - 1, circleCols - 1) As Double

            ' Compute allowed radius for each circle: min(distance to edge, half distance to neighbor centers)
            For row As Integer = 0 To circleRows - 1
                For col As Integer = 0 To circleCols - 1
                    Dim center3D = gridCenters(row, col)
                    ' Panel axes
                    Dim panelXdir As Vector3 = Vector3.Normalize(Lerp3DVec(cv1, cv2, 0.5) - Lerp3DVec(cv0, cv3, 0.5))
                    Dim panelYdir As Vector3 = Vector3.Normalize(Lerp3DVec(cv3, cv2, 0.5) - Lerp3DVec(cv0, cv1, 0.5))
                    ' Four panel edges as segments
                    Dim edgeA1 As Vector3 = cv0 : Dim edgeA2 As Vector3 = cv1 ' top
                    Dim edgeB1 As Vector3 = cv1 : Dim edgeB2 As Vector3 = cv2 ' right
                    Dim edgeC1 As Vector3 = cv2 : Dim edgeC2 As Vector3 = cv3 ' bottom
                    Dim edgeD1 As Vector3 = cv3 : Dim edgeD2 As Vector3 = cv0 ' left
                    ' Distance to each edge
                    Dim dA As Double = DistanceToSegment(center3D, edgeA1, edgeA2)
                    Dim dB As Double = DistanceToSegment(center3D, edgeB1, edgeB2)
                    Dim dC As Double = DistanceToSegment(center3D, edgeC1, edgeC2)
                    Dim dD As Double = DistanceToSegment(center3D, edgeD1, edgeD2)
                    Dim maxEdgeRadius As Double = Math.Min(Math.Min(dA, dB), Math.Min(dC, dD)) - epsilon

                    ' Distance to neighbors
                    Dim maxNeighborRadius As Double = Double.PositiveInfinity
                    For dRow As Integer = -1 To 1
                        For dCol As Integer = -1 To 1
                            If (dRow = 0 AndAlso dCol = 0) Then Continue For
                            Dim nRow As Integer = row + dRow
                            Dim nCol As Integer = col + dCol
                            If nRow < 0 OrElse nRow >= circleRows OrElse nCol < 0 OrElse nCol >= circleCols Then Continue For
                            Dim neighborCenter = gridCenters(nRow, nCol)
                            Dim dist = (center3D - neighborCenter).Length()
                            maxNeighborRadius = Math.Min(maxNeighborRadius, dist / 2 - epsilon)
                        Next
                    Next

                    Dim maxAllowedRadius = Math.Min(maxEdgeRadius, maxNeighborRadius)
                    Dim safeRadius As Double = Math.Max(Math.Min(maxRadius, maxAllowedRadius), 0)
                    Dim radius As Double
                    If safeRadius >= minRadius Then
                        radius = minRadius + rand.NextDouble() * (safeRadius - minRadius)
                    Else
                        radius = safeRadius
                    End If
                    circleRadii(row, col) = radius
                Next
            Next

            ' Now draw all circles
            For row As Integer = 0 To circleRows - 1
                For col As Integer = 0 To circleCols - 1
                    Dim center3D = gridCenters(row, col)
                    Dim radius = circleRadii(row, col)
                    Dim panelXdir As Vector3 = Vector3.Normalize(Lerp3DVec(cv1, cv2, 0.5) - Lerp3DVec(cv0, cv3, 0.5))
                    Dim panelYdir As Vector3 = Vector3.Normalize(Lerp3DVec(cv3, cv2, 0.5) - Lerp3DVec(cv0, cv1, 0.5))
                    Dim circleColor As ObjectColor = colorChoices(rand.Next(colorChoices.Count))
                    Dim circlePoints As Integer = Math.Max(12, CInt(Math.Round(2 * Math.PI * radius)))
                    For p As Integer = 0 To circlePoints - 1
                        Dim angle As Double = 2 * Math.PI * p / circlePoints
                        Dim dx As Double = Math.Cos(angle)
                        Dim dy As Double = Math.Sin(angle)
                        Dim offset3D = panelXdir * CSng(dx * radius) + panelYdir * CSng(dy * radius)
                        Dim pt As Vector3 = center3D + offset3D
                        Dim px As Integer = CInt(Math.Round(cubeCenter.X + pt.X))
                        Dim py As Integer = CInt(Math.Round(cubeCenter.Y + pt.Y))
                        Dim pz As Integer = CInt(Math.Round(cubeCenter.Z + pt.Z))
                        Dim lx As Single = CSng(px - cubeCenter.X)
                        Dim ly As Single = CSng(py - cubeCenter.Y)
                        Dim lz As Single = CSng(pz - cubeCenter.Z)
                        circlesLocalOffsets.Add(New Vector3(lx, ly, lz))
                        Dim key As Integer = api.AddMyObjectToFactory(px, py, pz, StructureIds.Circles)
                        circlesObjectIds.Add(key)
                        circlesColors.Add(circleColor)
                    Next
                Next
            Next

            Dim circlesObjIdsList As System.Collections.Immutable.ImmutableList(Of Integer) = Nothing
            If structureObjIds.TryGetValue(StructureIds.Circles, circlesObjIdsList) Then
                For i As Integer = 0 To circlesObjIdsList.Count - 1
                    Dim objId = circlesObjIdsList(i)
                    Dim obj As MyObject = Nothing
                    If objectDict.TryGetValue(objId, obj) Then
                        obj.ColorOverride = circlesColors(i)
                    End If
                Next
            End If

            initialized = True
        End Sub

        Private Function Lerp3DVec(a As Vector3, b As Vector3, t As Double) As Vector3
            Return a + (b - a) * CSng(t)
        End Function

        Private Function GetNonWhiteColors() As List(Of ObjectColor)
            Dim allVals = [Enum].GetValues(GetType(ObjectColor))
            Dim colors As New List(Of ObjectColor)
            For Each v As ObjectColor In allVals
                If v <> ObjectColor.White Then
                    colors.Add(v)
                End If
            Next
            Return colors
        End Function

        ' Minimum distance from point to segment ab
        Private Function DistanceToSegment(pt As Vector3, a As Vector3, b As Vector3) As Double
            Dim ab As Vector3 = b - a
            Dim abLen2 As Double = ab.LengthSquared()
            If abLen2 = 0 Then Return (pt - a).Length()
            Dim ap As Vector3 = pt - a
            Dim t As Double = Math.Max(0, Math.Min(1, Vector3.Dot(ap, ab) / abLen2))
            Dim closest As Vector3 = a + ab * CSng(t)
            Return (pt - closest).Length()
        End Function


        Public Sub RefreshObjectIdsAfterThinning(api As ICurrentApi)
            Dim objectDict = DirectCast(api.objectDictionary, ConcurrentDictionary(Of Integer, MyObject))

            ' Filter cubeObjectIds and cubeLocalOffsets
            Dim newCubeIds As New List(Of Integer)
            Dim newCubeOffsets As New List(Of Vector3)
            For i As Integer = 0 To cubeObjectIds.Count - 1
                If objectDict.ContainsKey(cubeObjectIds(i)) Then
                    newCubeIds.Add(cubeObjectIds(i))
                    newCubeOffsets.Add(cubeLocalOffsets(i))
                End If
            Next
            cubeObjectIds = newCubeIds
            cubeLocalOffsets = newCubeOffsets

            ' Filter highlightObjectIds and highlightLocalOffsets
            Dim newHighlightIds As New List(Of Integer)
            Dim newHighlightOffsets As New List(Of Vector3)
            For i As Integer = 0 To highlightObjectIds.Count - 1
                If objectDict.ContainsKey(highlightObjectIds(i)) Then
                    newHighlightIds.Add(highlightObjectIds(i))
                    newHighlightOffsets.Add(highlightLocalOffsets(i))
                End If
            Next
            highlightObjectIds = newHighlightIds
            highlightLocalOffsets = newHighlightOffsets

            ' Filter circlesObjectIds and circlesLocalOffsets
            Dim newCirclesIds As New List(Of Integer)
            Dim newCirclesOffsets As New List(Of Vector3)
            For i As Integer = 0 To circlesObjectIds.Count - 1
                If objectDict.ContainsKey(circlesObjectIds(i)) Then
                    newCirclesIds.Add(circlesObjectIds(i))
                    newCirclesOffsets.Add(circlesLocalOffsets(i))
                End If
            Next
            circlesObjectIds = newCirclesIds
            circlesLocalOffsets = newCirclesOffsets
        End Sub



    End Class

    Private Shared Function RandomUnitVector(rand As Random) As Vector3
        Dim u = rand.NextDouble()
        Dim v = rand.NextDouble()
        Dim theta = 2 * Math.PI * u
        Dim phi = Math.Acos(2 * v - 1)
        Dim x = Math.Sin(phi) * Math.Cos(theta)
        Dim y = Math.Sin(phi) * Math.Sin(theta)
        Dim z = Math.Cos(phi)
        Return New Vector3(CSng(x), CSng(y), CSng(z))
    End Function



End Class