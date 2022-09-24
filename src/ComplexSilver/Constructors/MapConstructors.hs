module ComplexSilver.Constructors.MapConstructors where

import Apecs
import Apecs.Physics
import ComplexSilver.Components.CameraTarget
import ComplexSilver.Components.Pickup
import ComplexSilver.Components.Player
import ComplexSilver.Components.TerminalVelocity
import ComplexSilver.Constants
import ComplexSilver.Sprites
import ComplexSilver.Systems.Grounded
import ComplexSilver.Systems.Pickup
import ComplexSilver.Systems.Spritesheet
import ComplexSilver.World
import Data.Bits (Bits (zeroBits), (.&.))
import Data.Functor
import Data.Word

consFail :: WVec -> SystemT World IO Entity
consFail pos = do
  newEntity ()

consSolid :: Int -> WVec -> SystemT World IO Entity
consSolid sprite_id pos = do
  sprite <- sprite_sheet_0 sprite_id
  entity <-
    newEntity
      ( StaticBody,
        Position pos,
        -- StaticBody,
        defaultAnimation
          { animReel = [sprite],
            animSpeed = 0.0,
            animState = 0,
            animTime = 0.0
          }
      )
  rigidBody <-
    newEntity
      ( Shape entity $ cRectangle 8,
        solidFilter
      )
  return entity

consPickup :: Int -> SystemT World IO () -> WVec -> SystemT World IO Entity
consPickup spriteId onPickup position = do
  liftIO $ print $ "making pickup" <> show spriteId
  sprite <- sprite_sheet_0 spriteId
  entity <-
    newEntity
      ( Sensor True,
        DynamicBody,
        Position position,
        defaultAnimation
          { animReel = [sprite],
            animSpeed = 1,
            animTime = 0,
            animState = 0
          }
      )
  liftIO $ print $ show entity
  rigidBody <-
    newEntity
      ( Shape entity $ cRectangle 8,
        pickupFilter
      )
  return $ entity

consPlayer :: WVec -> SystemT World IO Entity
consPlayer position = do
  sprites <- sequence $ sprite_sheet_0 <$> [1, 2]
  player <-
    newEntity
      ( DynamicBody,
        Position position,
        PlayerMovement
          { movingRight = False,
            movingLeft = False,
            jumping = False,
            jumpTimer = 0
          },
        CameraTarget 1,
        defaultAnimation
          { animReel = sprites,
            animSpeed = 4,
            animTime = 0,
            animState = 0
          },
        Grounded False,
        TerminalVelocity $ -256
      )
  player
    $= NotHoldingAnything
      { equipTo = player
      }
  precollisionCallback <-
    mkPreSolveCB $
      \col@Collision
         { collisionShapeA = shapeAEntity,
           collisionShapeB = shapeBEntity
         } -> do
          CollisionFilter
            { filterCategories = bCategory
            } <-
            get shapeBEntity
          CollisionFilter
            { filterMask = aMask
            } <-
            get shapeAEntity
          lift $ print (bCategory, aMask)
          if (bCategory .&. aMask) == zeroBits
            then return False
            else do
              groundedCollisionCallback player col
              return True

  set
    player
    ( defaultHandler
        { preSolveCB = Just precollisionCallback
        }
    )
  newEntity
    ( Shape player $ cCircle 4,
      playerFilter
    )
  -- x <- newEntity ()
  -- newEntity $ Constraint player x (RotaryLimitJoint 0 0)
  return player

consFromTileId :: Word32 -> WVec -> Maybe (SystemT World IO Entity)
consFromTileId 0 = const Nothing
consFromTileId 2 = Just . consPlayer
consFromTileId 7 = Just . consPickup 6 (return ())
consFromTileId 9 = Just . consPickup 8 (return ())
consFromTileId 14 = Just . consPickup 13 (return ())
consFromTileId 25 = Just . consPickup 25 (return ())
consFromTileId 20 = Just . consSolid 20
consFromTileId 28 = Just . consSolid 28
consFromTileId n =
  Just . \p -> do
    liftIO $ print $ "Attempt to construct unlisted tile " <> show n <> " at " <> show p
    consFail p