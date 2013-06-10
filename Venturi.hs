merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  case compare x y of
    LT -> x : (merge xs (y:ys))
    EQ -> x : (merge xs ys) 
    GT -> y : (merge (x:xs) ys)

data People a = VIP a (People a) | Crowd [a] 
merge' :: Ord a => People a -> People a -> People a 
merge' (VIP x xt) ys         = VIP x (merge' xt ys) 
merge' (Crowd xs) (Crowd ys) = Crowd (merge xs ys) 
merge' xs@(Crowd (x:xt)) ys@(VIP y yt) 
     | x <= y    = VIP x (merge' (Crowd xt) ys) 
     | otherwise = VIP y (merge' xs yt) 
merge' (Crowd []) x = x 
diff xs@(x:xt) ys@(y:yt) =  
      case compare x y of  
           LT -> x : (diff xt ys)  
           EQ -> diff xt yt  
           GT -> diff xs yt 
wrap [] = Crowd [] 
wrap (x:xs) = VIP x $ Crowd xs 
unwrap (VIP x xt) = x: unwrap xt 
unwrap (Crowd l) = l 
  
venturi xss = unwrap $ foldr1 merge' xss 
primes = (2:) $ diff [3,5..] $ venturi $  
        [wrap (map (*x) primes)| x<-[3,5..]] 
  
first_sums a b = venturi [wrap (map (+ x) a)|x<-b] 

four_three_factors = venturi [wrap (map (*x) primes')|x<-primes'] 
                    where primes' = filter (\x->mod x 4==3) primes
