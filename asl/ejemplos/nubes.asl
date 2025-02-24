scene 500 500 #51,194,226

red : Color = #255,0,0
white1 : Color = #255,255,255
white2 : Color = #167,198,205

-- definicion de nave
ship : Image = rot (triang 40.0 60.0 60.0 red) -90.0
subir : Action = move {0.0, 100.0} 2.0
bajar : Action = move {0.0, -100.0} 2.0

ship_anim : Anim = place ship {0.0, -100.0} 0.0 1.0
ship_anim << loop ( subir ; bajar ) 3

-- definicion de nube

cloud_1 : Image = circle 40.0 0.0 full white1
cloud_2 : Image = offset cloud_1 (resize cloud_1 0.75) {40.0, -20.0}
cloud_3 : Image = offset cloud_2 (resize cloud_1 0.75) {-40.0, -20.0}
cloud : Image = offset cloud_3 (rect 80.0 20.0 0.0 full white1) {0.0, -40.0} 

-- definicion de nubes de frente
clouds_front_1 : Image = bind cloud {100.0, 130.0} cloud {120.0, -150.0}
clouds_front_2 : Image = offset clouds_front_1 cloud {-89.0, -167.0}
clouds_front_3 : Image = offset clouds_front_2 cloud {-150.0, 100.0}
clouds_front_4 : Image = offset clouds_front_3 cloud {-250.0, -60.0}
clouds_front : Image = offset clouds_front_4 cloud {-300.0, 10.0}
clouds_front_final : Image = offset (offset clouds_front clouds_front {-400.0, 0.0}) clouds_front {-800.0, 0.0}

front_anim : Anim = place clouds_front_final {-450.0, 50.0} 0.0 1.0
front_anim << move {1500.0, 0.0} 15.0

-- definicion de nubes de fondo
cloud_back : Image = resize (paint cloud white2) 0.75
clouds_back_1 : Image = bind cloud_back {-114.0, 40.0} cloud_back {50.0, -10.0}
clouds_back_2 : Image = offset clouds_back_1 cloud_back {0.0, 120.0}
clouds_back_3 : Image = offset clouds_back_2 cloud_back {-150.0, -60.0}
clouds_back_4 : Image = offset clouds_back_3 cloud_back {-300.0, 10.0}
clouds_back : Image = offset clouds_back_4 cloud_back {-250.0, -120.0}
clouds_back_final : Image = offset clouds_back clouds_back {-400.0, 0.0}

back_anim : Anim = place clouds_back_final {-400.0, 50.0} 0.0 1.0
back_anim << move {1000.0, 0.0} 15.0

play [ back_anim, ship_anim, front_anim ]