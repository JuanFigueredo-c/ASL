scene 1350 700 #0,0,0

dark_blue : Color = #65,51,255
green : Color = #79,255,73
orange1 : Color = #255,146,36
orange2 : Color = #246,110,42
cream1 : Color = #243,214,157
cream2 : Color = #239,212,137
gray : Color = #132,132,132
yellow1 : Color = #209,236,75
yellow2 : Color = #255,236,58

neptune : Image = circle 22.5 0.0 full #65,51,255

urane : Image = circle 22.5 0.0 full green

jupiter_base : Image = circle 30.0 0.0 full orange1
jupiter_eye : Image = stack (circle 5.0 0.0 full orange2) (circle 15.0 0.0 full cream1)
jupiter : Image = offset jupiter_base jupiter_eye {5.0, 5.0}

saturn_rings : Image = circle 27.5 10.0 outline cream2
saturn : Image = stack (circle 20.0 0.0 full cream1) saturn_rings

mars : Image = circle 15.0 0.0 full orange2

continent : Image = triang 15.0 18.0 18.0 green
earth : Image = stack continent (circle 15.0 0.0 full dark_blue)

venus : Image = circle 15.0 0.0 full yellow1

mercury : Image = circle 5.0 0.0 full gray

sun : Image = stack (circle 35.0 0.0 full yellow2) (poly 6 45.0 orange1) 

neptune_orbit : Anim = place neptune {0.0, 327.5} 0.0 1.0
urane_orbit : Anim = place urane {0.0, 282.5} 0.0 1.0
saturn_orbit : Anim = place saturn {0.0, 232.5} 0.0 1.0
jupiter_orbit : Anim = place jupiter {0.0, 175.5} 0.0 1.0
mars_orbit : Anim = place mars {0.0, 130.0} 0.0 1.0
earth_orbit : Anim = place earth {0.0, 100.0} 0.0 1.0
venus_orbit : Anim = place venus {0.0, 70.0} 0.0 1.0
mercury_orbit : Anim = place mercury {0.0, 50.0} 0.0 1.0
sun_orbit : Anim = place sun {0.0, 0.0} 0.0 1.0


neptune_orbit << loop (orbit {0.0, 0.0} 360.0 6.0) 5  || rotate 360.0 2.0
urane_orbit << loop (orbit {0.0, 0.0} -360.0 5.0) 6 || rotate 360.0 2.0
jupiter_orbit << loop (orbit {0.0, 0.0} 360.0 10.0) 3 || rotate 360.0 2.0
saturn_orbit << loop (orbit {0.0, 0.0} 360.0 5.0) 6 || rotate 360.0 2.0
mars_orbit << loop (orbit {0.0, 0.0} -360.0 3.0) 10 || rotate 360.0 2.0
earth_orbit << loop (orbit {0.0, 0.0} 360.0 3.0) 10 || rotate 360.0 2.0
venus_orbit << loop (orbit {0.0, 0.0} 360.0 2.0) 15 || rotate 360.0 2.0
mercury_orbit << loop (orbit {0.0, 0.0} 360.0 1.0) 30 || rotate 360.0 2.0
sun_orbit << loop (rotate 360.0 10.0) 3 

play [ neptune_orbit, urane_orbit, jupiter_orbit, 
     saturn_orbit, mars_orbit, earth_orbit, venus_orbit,
     mercury_orbit, saturn_orbit, sun_orbit]
