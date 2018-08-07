module Games 
    (   gravity
    ,   Rigidbody(..)
    ,   rigidbodyWithGravity
    ,   addForce
    ,   addSpeed
    ,   addImpulse
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
        {   acceleration     :: Vector
        ,   velocity         :: Vector
        ,   angularVelocity  :: Float
        ,   mass             :: Float
        }

    rigidbodyWithGravity :: Float -> Maybe Vector -> Rigidbody
    rigidbodyWithGravity mass mv = case mv of
                                Just v  -> Rigidbody gravity    v  0 mass  
                                Nothing -> Rigidbody gravity (0,0) 0 mass

    addForce :: Rigidbody -> Vector -> Rigidbody
    addForce rb pow = rb { acceleration = addV (acceleration rb) pow }
    
    -- 物理法則には従っていない(従わせると一切回転しないため)
    addImpulse :: Rigidbody -> Vector -> Float -> Float -> Rigidbody
    addImpulse rb pow dist r = rb
        {   velocity        = addV (velocity rb) $ mulSV (1/(mass rb)) pow
        ,   angularVelocity = (angularVelocity rb) + 
                              dist * (x pow) * 2 / (mass rb) / dist / dist  
        }

    addSpeed :: Rigidbody -> Vector -> Rigidbody
    addSpeed rb speed = rb { velocity = addV (velocity rb) speed }

    updateVelocity :: Rigidbody -> Float -> Rigidbody
    updateVelocity rb dt = rb 
        {   velocity = addV (velocity rb) . mulSV dt $ acceleration rb 
        }

    updateTransform :: Transform -> Rigidbody -> Float -> Transform
    updateTransform tf rb dt = tf
        {   pos = addV (pos tf) . mulSV dt $ velocity rb
        ,   rot = (rot tf) + (dt * angularVelocity rb)
        }

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
