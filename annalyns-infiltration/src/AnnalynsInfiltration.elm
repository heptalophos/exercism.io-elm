module AnnalynsInfiltration exposing 
    ( 
      canFastAttack, 
      canFreePrisoner, 
      canSignalPrisoner, 
      canSpy, 
      stealthAttackDamage 
    )


canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    case knightIsAwake of
        True  -> False
        False -> True


canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    case (knightIsAwake, archerIsAwake, prisonerIsAwake) of
         (True, _, _) -> True
         (_, True, _) -> True
         (_, _, True) -> True
         (_, _, _)    -> False


canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    case (archerIsAwake, prisonerIsAwake) of
         (False, True) -> True
         (_, _)        -> False


canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    case (knightIsAwake, archerIsAwake, prisonerIsAwake) of
         (False, False, True) -> True
         (_, False, _)        -> petDogIsPresent
         (_, _, _)            -> False
        

stealthAttackDamage : Bool -> Int
stealthAttackDamage annalynIsDetected =
    case annalynIsDetected of
        True  -> 7
        False -> 12
