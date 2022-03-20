module Notes where

--high-score: b4: 16 90%
--            b3: 11
--            b5: Median 44

--Improvements:
  --append the other way around in updateList
  --clean up game engine and add growth-function :: Score -> Length
  --incorporate phases into ca model somehow
    --make phaseify a policy-transformer that restricts by taking the next best action
  --extra completion reward? show completion rate
  --automate config-adaptations, ff
  --seperate nn params from activationfunc which stays constant
  --solve almost-cycle

  --show variance, change of population

  --random snake starting directions
    --not yet an issue


  --make DNNs storable with data.binary
  --check what takes so long aka profiling
  --random monad instead of contingent
  --make Apple-randomness with Contingent-Monad (no?)

{- 
Mit entsprechendem Training findet der Perceiver auf Karten mit n <=5 zuverlässig einen Hamiltonkreis
Z.b. mit Rate [0.5, 0.01, 1] und einer dreiknotigen Zwischenebene
-}