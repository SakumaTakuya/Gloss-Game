module Main where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import Graphics.Gloss.Data.Vector
    import Data
    import Games

    {- - - - - - - - - - - }
          Displayの設定
    { - - - - - - - - - - -}

    windowWidth, windowHeight :: Num a => a
    windowWidth = 640
    windowHeight = 480

    window :: Display
    window = InWindow "Hello World" (windowWidth, windowHeight) (100, 100)

    {- - - - - - - - - - - - }
          シミュレーション
    { - - - - - - - - - - - -}

    -- オブジェクトをクリックしたときの力
    power :: Float
    power = 10

    size :: Vector
    size = (50, 50)

    radius :: Float
    radius = 50 * 1.4
     
    initialize :: GameObject
    initialize = GameObject 
        (Transform (0,100) 0 (1,1)) 
        (mulSV 1.5 size)
        (rigidbodyWithGravity 1 $ Just (0, 100))

    draw :: GameObject -> Picture
    draw go = pictures 
        [   trs (tf go) $ 
                rectangleSolid  (width size) 
                                (height size)
        ,   translate (-windowWidth) 0 . scale 0.5 0.5 $ 
                text $          "Vel:" ++ (show (velocity $ body go)) ++ 
                                "\nAcc:" ++ (show (acceleration $ body go))
        ]

    update :: Event -> GameObject -> GameObject
    update (EventKey key ks _ mouse) go = updateWithClick key ks mouse go
    update _ go = go

    updateWithClick :: Key -> KeyState -> Vector -> GameObject -> GameObject
    updateWithClick (MouseButton LeftButton) ks mouse go = 
        if ks == Down 
        then hit mouse go 
        else go
    updateWithClick _ _ _ go = go

    hit :: Vector -> GameObject -> GameObject
    hit mouse go | inObject go mouse = 
                    let d = diffV (pos $ tf go) mouse 
                    in  go  {   body = addImpulse   (body go) 
                                                    (mulSV (-power) d)
                                                    (magV d)
                                                    radius
                            }
                 | otherwise = go

    next :: Float -> GameObject -> GameObject
    next dt go =
        let p = pos $ tf go
            v = velocity $ body go
            newvel = 
                if  (x p) >= (windowWidth/2) || (x p) <= -(windowWidth/2)
                then (-(x $ v), (y $ v))
                else v
        in  simulatePhysics dt go
            {   body = (body go){ velocity = newvel }
            }

    main :: IO ()
    main = play window white 60 initialize draw update next 