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
     
    initialize :: GameObject
    initialize = GameObject 
        (Transform (0,100) 0 (1,1)) 
        (mulSV 1.5 size)
        (rigidbodyWithGravity $ Just (0, 100))

    draw :: GameObject -> Picture
    draw go = pictures 
        [   trs (tf go) $ 
                rectangleSolid  (width size) 
                                (height size)
        ,   translate (-windowWidth) 0 . scale 0.5 0.5 $ 
                text $          "Vel:" ++ (show (vel $ body go)) ++ 
                                "Acc:" ++ (show (acc $ body go))
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
                    in  go {body = addSpeed (body go) (mulSV (-power) d)}
                 | otherwise = go

    next :: Float -> GameObject -> GameObject
    next dt go = simulatePhysics dt go

    main :: IO ()
    main = play window white 60 initialize draw update next 