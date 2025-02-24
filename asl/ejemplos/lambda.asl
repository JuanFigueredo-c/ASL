scene 1300 600 #255,255,230

colorLambda : Color = #0,0,0
red : Color = #255,0,0

linea_fondo1 : Image = line 1400.0 0.0 40.0 red 
linea_fondo2 : Image = stack linea_fondo1 (rot linea_fondo1 30.0) 
linea_fondo3 : Image = stack linea_fondo2 (rot linea_fondo1 60.0) 
linea_fondo4 : Image = stack linea_fondo3 (rot linea_fondo1 90.0) 
linea_fondo5 : Image = stack linea_fondo4 (rot linea_fondo1 120.0) 
fondo : Image = stack linea_fondo5 (rot linea_fondo1 150.0) 
circ : Image = circle 70.0 0.0 full red
background : Image = stack circ fondo

animFondo : Anim = place background {0.0, 0.0} 0.0 1.0
animFondo << loop rotate -360.0 9.0  5

line1 : Image = line 45.0 65.0 8.0 colorLambda
line2 : Image = offset line1 (line 30.0 110.0 8.0 colorLambda) {-8.0, -6.5}
lambda_ : Image = offset line2 (line 10.5 0.0 8.0 colorLambda) {-11.0 ,18.0}

lambda : Image = resize (paint lambda_ #255,255,255) 2.0


animLambda : Anim = place lambda {8.0, 0.0} 0.0 1.0

play [ animFondo, animLambda ]