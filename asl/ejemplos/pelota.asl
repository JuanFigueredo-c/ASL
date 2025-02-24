scene 1200 650 #255,255,255

red : Color = #255,0,0
yellow : Color = #255,255,0
black : Color = #0,0,0

pelota : Image = stack (line 40.0 0.0 4.0 yellow) (circle 20.0 0.0 full red) 

piso : Image = line 1200.0 0.0 3.0 black

piso_anim : Anim = place piso {0.0, -20.0} 0.0 1.0
pelota_anim : Anim = place pelota {570.0, 0.0} 0.0 1.0

bounce1 : Action = orbit {370.0, 0.0} 180.0 1.0
bounce2 : Action = orbit {0.0, 0.0} 180.0 0.75
bounce3 : Action = orbit {-270.0, 0.0} 180.0 0.5
bounce4 : Action = orbit {-420.0, 0.0} 180.0 0.25
bounce5 : Action = orbit {-505.0, 0.0} 180.0 0.18
bounce6 : Action = orbit {-555.0, 0.0} 180.0 0.1
back : Action = move {570.0, 0.0} 3.0

bounces : Action = bounce1 ; bounce2 ; bounce3 ; bounce4 ; bounce5 ;  bounce6 || rotate -720.0 0.75
movement : Action = static 1.0 ; bounces ;  static 1.0 ; (back || rotate 540.0 2.0)

pelota_anim << loop movement 4

play [ pelota_anim , piso_anim ]