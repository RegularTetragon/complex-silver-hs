module ComplexSilver.Systems.Pickup where
import Apecs
import ComplexSilver.World
import Apecs.Physics
import ComplexSilver.Components.Pickup
import ComplexSilver.Constants
import ComplexSilver.Sprites (Animation(Animation))

unexposedError = error "Apparently unexposed apecs field filterGroup evaluated on pickup point check"

pickupPointFilter = CollisionFilter {
    filterGroup = -1,
    filterCategories = colLayerPickupScanner,
    filterMask = colLayerPickup
}

pickupOnPlayer :: SystemT World IO ()
pickupOnPlayer = do
    cmapM $ \(Position p, pickupController) -> do
        case pickupController of
            NotHoldingAnything equipTo -> do
                pqResult <- pointQuery (p + V2 8 0) 4 pickupPointFilter
                case pqResult of
                    Nothing -> do
                        liftIO $ print "Nothing to pick up"
                        return $ NotHoldingAnything equipTo
                    Just (PointQueryResult pqShape pqPoint pqDistance pqGradient) -> do
                        -- Extract the pickup's physics object (pickup logic must be on rigid body)
                        Shape pqPhysicsEntity _ <- get pqShape
                        -- Create pin constraint
                        pqPhysicsEntity $= KinematicBody
                        weldConstraint <- newEntity (
                            -- Constraint pqPhysicsEntity equipTo $ PivotJoint p
                            )

                        -- Set item to held with an empty action queue
                        pqPhysicsEntity $= HeldPickup equipTo []
                        pqPhysicsEntity $= Mass 0.001
                        liftIO $ print $ "Equipping" <> show pqPhysicsEntity <> "to" <> show equipTo
                        -- On the current player mark that we are currently holding a pickup
                        return $ HoldingPickup {
                            holder = equipTo,
                            holdingItem = pqPhysicsEntity
                            }
            HoldingPickup holder holdingItem -> do
                liftIO $ print $ "Throwing Pickup" <> show holdingItem
                -- Break the weld
                -- destroy physicsWeld (undefined :: Proxy Constraint)
                -- Set item to unheld
                holdingItem $= DynamicBody
                holdingItem $= UnheldPickup
                -- Throw the item
                holdingItem $= Velocity (V2 300 100)
                holdingItem $= AngularVelocity (-720)
                return $ NotHoldingAnything {equipTo = holder}


stepHeldPickups :: Double -> SystemT World IO ()
stepHeldPickups deltaTime =  do
    cmapM $ \pickupController -> do
        case pickupController of
            HoldingPickup holder holdingItem -> do
                Position p <- get holder
                holdingItem $= Position (p + V2 8 0)
            _ -> return ()