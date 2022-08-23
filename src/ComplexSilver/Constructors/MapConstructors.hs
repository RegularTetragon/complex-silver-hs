module ComplexSilver.Constructors.MapConstructors where
import Apecs
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Sprites
import ComplexSilver.Components.Player
import Data.Word
import ComplexSilver.Systems.Spritesheet
import ComplexSilver.Components.CameraTarget
import ComplexSilver.Systems.Grounded
import Data.Functor

consFail :: WVec -> SystemT World IO Entity
consFail pos = do
    newEntity ()
consSolid :: Int -> WVec -> SystemT World IO Entity
consSolid sprite_id pos = do
    sprite <- sprite_sheet_0 sprite_id
    entity <- newEntity (
        StaticBody,
        Position pos,
        -- StaticBody,
        Animation {
            animReel = [sprite],
            animSpeed = 0.0,
            animState = 0,
            animTime = 0.0
        }
        )
    rigidBody <- newEntity $ Shape entity $ cRectangle 12

    return entity

consPickup :: Int -> SystemT World IO ()-> WVec -> SystemT World IO Entity
consPickup spriteId onPickup position = do
    sprite <- sprite_sheet_0 spriteId
    entity <- newEntity (
        StaticBody,
        Position position,
        Sensor False,
        Animation {
            animReel = [sprite],
            animSpeed = 0,
            animTime = 0,
            animState = 0
        }
        )
    rigidBody <- newEntity $ Shape entity $ cRectangle 8
    return $ entity

consPlayer :: WVec -> SystemT World IO Entity
consPlayer position = do
    sprites <- sequence $ sprite_sheet_0 <$> [1,2]
    player <- newEntity
        (
            DynamicBody,
            Position position,
            PlayerMovement {
                movingRight = False,
                movingLeft = False,
                jumping = False,
                jumpTimer = 0
            },
            CameraTarget 1,
            Animation {
                animReel = sprites,
                animSpeed = 4,
                animTime = 0,
                animState = 0
            },
            Grounded False
        )
    precollisionCallback <- mkPreSolveCB $ \col-> do
        groundedCollisionCallback player col
        return True

    set player (defaultHandler {
        preSolveCB = Just precollisionCallback
    })
    newEntity $ Shape player $ cCircle 4
    x <- newEntity ()
    newEntity $ Constraint player x (RotaryLimitJoint 0 0)
    return player

consFromTileId :: Word32 -> WVec -> Maybe (SystemT World IO Entity)
consFromTileId 0  = const Nothing
consFromTileId 2  = Just . consPlayer
consFromTileId 7  = Just . consPickup 6 (return ())
consFromTileId 9  = Just . consPickup 8 (return ())
consFromTileId 14 = Just . consPickup 13 (return ())
consFromTileId 25 = Just . consPickup 25 (return ())
consFromTileId 20 = Just . consSolid 20
consFromTileId 28 = Just . consSolid 28
consFromTileId n = Just . \p->do
    liftIO $ print $ "Attempt to construct unlisted tile " <> show n <> " at " <> show p
    consFail p