module Data where

    import Graphics.Gloss
    import Graphics.Gloss.Data.Vector

    x :: Vector -> Float
    x v = fst v

    y :: Vector -> Float
    y v = snd v

    width :: Vector -> Float
    width v = fst v

    height :: Vector -> Float
    height v = snd v

    addV :: Vector -> Vector -> Vector
    addV (x1, y1) (x2, y2) = (x1+x2, y1+y2)

    diffV :: Vector -> Vector -> Vector
    diffV (xf, yf) (xt, yt) = (xt-xf, yt-yf)

    data Transform = Transform 
        { pos :: Vector
        , rot :: Float
        , sca :: Vector }

    --ワールド座標からオブジェクト座標に変換する
    trs :: Transform -> (Picture -> Picture)
    trs t = translate (x $ pos t) (y $ pos t) . rotate (rot t) . scale (x $ sca t) (y $ sca t)

    --オブジェクト座標上の点を作成する
    trsV :: Vector -> Transform -> Vector
    trsV (vx, vy) t = 
        let rV = rotateV (rot t) ((fst $ sca t) * vx, (snd $ sca t) * vy)
        in  addV rV (pos t)

    --あるTransformに対するローカル座標を取り出す
    localV :: Vector -> Transform -> Vector
    localV v t =
        let rV = rotateV (-(rot t)) . addV (mulSV (-1) (pos t)) $ v
        in ((x rV) / (x $ sca t), (y rV) / (y $ sca t))
