module Games 
    (   gravity
    ,   Rigidbody(..)
    ,   rigidbodyWithGravity
    ,   addForce
    ,   addSpeed
    ,   GameObject(..)
    ,   simulatePhysics
    ,   inObject 
    )
where

    import Graphics.Gloss
    import Graphics.Gloss.Data.Vector
    import Data

    gravity :: Vector
    gravity = (0, -9.8 * 5) --リアルな重力だと小さすぎたため

    data Rigidbody = Rigidbody
        {   acc     :: Vector
        ,   vel     :: Vector
        }

    rigidbodyWithGravity :: Maybe Vector -> Rigidbody
    rigidbodyWithGravity mv = case mv of
                                Just v  -> Rigidbody gravity v
                                Nothing -> Rigidbody gravity (0,0)

    addForce :: Rigidbody -> Vector -> Rigidbody
    addForce rb pow = rb { acc = addV (acc rb) pow }
    
    addSpeed :: Rigidbody -> Vector -> Rigidbody
    addSpeed rb speed = rb { vel = addV (vel rb) speed }

    updateVelocity :: Rigidbody -> Float -> Rigidbody
    updateVelocity rb dt = rb { vel = addV (vel rb) . mulSV dt $ acc rb }

    updateTransform :: Transform -> Rigidbody -> Float -> Transform
    updateTransform tf rb dt = tf{ pos = addV (pos tf) . mulSV dt $ vel rb}

    data GameObject = GameObject
        {   tf      :: Transform
        ,   bounds  :: Vector
        ,   body    :: Rigidbody 
        }

    simulatePhysics :: Float -> GameObject -> GameObject
    simulatePhysics dt go = 
        let newrb = updateVelocity (body go) dt
            newtf = updateTransform (tf go) newrb dt
        in  go
            {   tf = newtf
            ,   body = newrb 
            }

    inObject :: GameObject -> Vector -> Bool
    inObject go p = 
        let local = localV p (tf go)
        in  if  ( (width  $ bounds go)/2) >= (x local) && 
                ( (height $ bounds go)/2) >= (y local) &&
                (-(width  $ bounds go)/2) <= (x local) && 
                (-(height $ bounds go)/2) <= (y local)
            then True
            else False
