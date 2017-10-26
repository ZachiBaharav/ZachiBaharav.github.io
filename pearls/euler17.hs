{-
Number letter counts

Problem 17

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there 
are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
The use of "and" when writing out numbers is in compliance with British usage.
Solution:
Brute force.

-}

-- answer: 21124

module E17 where 

units = ["one","two","three","four","five","six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

l1_9 = length $ concat units
l10_19 = length $ concat teens
l20_99 = (length $ concat tens)*10   + 8*l1_9

l1_99 = (l1_9+l10_19+l20_99)

l100_999 = ( l1_9 + 9*(length "hundred") ) +     99*l1_9 +   9*99*(length "hundredand") + 9*l1_99 
l1000 = length "onethousand"

ans = l1_99 + l100_999 + l1000

main = do
    print $ ans
