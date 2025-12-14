
&nbsp;&nbsp;Satellite Cubes Plugin Configuration

**IMPORTANT:** You must adjust `orbitCenter` coordinates to match your world space. 

---

&nbsp;&nbsp;Orbit Settings

  Parameter    |  Default           |       Range | Description
---------------|--------------------|-------------|-------------
 `orbitCenter` | `(-250, 155, -78)` | Any Vector3 | **Must adjust** - 3D center point the main cube orbits around
 `orbitRadius` | `1700`             | >0          | Orbit radius in world units; larger = wider orbit
 `orbitSpeed`  | `Math.PI / 90`     | >0          | Angular speed (rad/s); lower = slower orbit
 `orbitPhase`  | `0`                | 0 to 2PI    | Starting position in orbit

--- 

&nbsp;&nbsp;Orbit Tilt

  Parameter          |  Default      | Range       | Description
---------------------|---------------|-------------|-------------
 `orbitTiltBaseAxis` | `(1, 0, 0)`   | Any Vector3 | Axis the orbit plane tilts around
 `orbitTiltSpeed`    | `Math.PI / 6` | >0          | Tilt oscillation speed (rad/s)
 `orbitTiltMaxAngle` | `Math.PI`     | 0 to PI     | Maximum tilt angle; 0 = no tilt
 `baseOrbitAxis`     | `(0, 1, 0)`   | Any Vector3 | "Up" axis for orbit plane

---

&nbsp;&nbsp;Cube Self-Rotation

  Parameter       |  Default                     | Range       | Description
------------------|------------------------------|-------------|-------------
 `primaryAxis`    | `(0, 1, 0)`                  | Any Vector3 | Primary spin axis
 `primarySpeed`   | `4 * Math.PI * 2 / 60`       | >0          | Spin speed around primary axis (rad/s)
 `secondaryAxis`  | `(1, 0, 0)`                  | Any Vector3 | Secondary spin axis
 `secondarySpeed` | `(4 * Math.PI * 2 / 60) / 2` | >0          | Spin speed around secondary axis (rad/s)

---

&nbsp;&nbsp;Satellites

  Parameter               | Default      | Range     | Description
--------------------------|--------------|-----------|-------------
 `satelliteCount`         | `8`          | >=0       | Number of orbiting satellite cubes
 `satelliteSizeRatio`     | `1.0 / -1.5` | 0 < x < 1 | Satellite size relative to main cube
 `satelliteDistanceRatio` | `4.2`        | >0        | Satellite orbit distance multiplier

---

&nbsp;&nbsp;Collision

  Parameter              | Default | Description
-------------------------|---------|-------------
 `collisionImmunityTime` | `3.5`   | Seconds of immunity after satellite collision

---

&nbsp;&nbsp;Internal

  Parameter       | Default | Description
------------------|---------|-------------
 `CubeStructBase` | `55555` | Base structure ID for cube objects
