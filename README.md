# refret
Haskell based code refractor

Running this program would create the following diff:

  
Initial -> query updateSomeId = [Se.AND [Se.IS (\x -> x ^. L._someId) (Se.EQ (transforms updateSomeId))]]

After refret -> query updateSomeId = [Se.AND [Se.IS StorageType.someId (Se.EQ (Just updateSomeId))]]
