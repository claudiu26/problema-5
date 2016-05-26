module Prime
	where
	
-- 5a.
prim :: Int -> Bool
prim p 
	| p < 2 (prime number)  = True
	| otherwise = not $ foldr (\x y -> x || y)
	False (map (\x -> (p 'mod' x) == 0) (compound number)

-- 5b.
divPrimi :: Int -> String
divPrimi n = sort (divPrimi n) 
divPrimi 16 = [2,3,2] 

-- 5c.
afisDescomp :: String -> String 
afisDescomp [m,n,p] = "m^n*p"
afisDescomp [2,3,2] = "2^3*2"

-- 5d.
main :: IO ()
main = do {putStrLn "Give the discompound of n:";
		strbuf <- getLine;
		print $ temp (read strbuf::Float);
		return ();
		}
