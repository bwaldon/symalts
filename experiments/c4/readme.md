# INTERPRETING THE RESULTS 

**"trial_type":** “response” or “filler   
**"primeType”:** expression type, 0 (some), 1 (number), 2 (ad hoc)  
**"primeStrength”:** priming condition, 0 (weak), 1 (strong), 2 (exhaustive), 3 (baseline)  
**"pOSentence”:** prime one sentence (e.g. Some of the symbols are stars)   
**"pOSymbols”:** array of three numbers, 1st number corresponds to the symbol that is named in the prompt, 2nd and 3rd are alternatives 0(♦), 1(♣), 2(✓), 3(♠), 4(♥), 5(◼), 6(★), 7(●), 8(♩), 9(▲), in baseline condition: 1st number is the right answer to the math question  
**"pOwrongAnswer”:** array of three numbers, always [0,0,0] except baseline condition, then 1st number is the wrong answer to the math question   
**"pOChoice":** participant’s choice for prime one, “0”(left) or “1” (right)    
**"goodPOChoice":** right answer to prime one, “0”(left) or “1”(right), (this is the index of 1 in primeOShuffle)    
**"primeOShuffle":** array of 0 and 1 showing the order of correct-incorrrect image (1 is the correct one), [0,1] or [1,0]   
**"correctPOChoice”:** true if participant gave right answer to prime one (if pOCHoice == goodPOChoice)  
**"pTSentence”:** (same as above for prime two)  
**"pTSymbols":**  
**"pTwrongAnswer":**   
**"pTChoice":**  
**"goodPTChoice":**   
**"primeTShuffle":**  
**"correctPTChoice":**  
**"correctPChoices”:** true if participant gave the right answer to both primes  
**"responseSentence”:** response sentence (e.g.Some of the symbols are diamonds)  
**"responseSymbols”:** array of three numbers corresponding to symbols in the array above
**"responseChoice":** participant’s choice to response trial, “0”(weak-left), “1”(better picture-right)  
**"rt":** response time

# TESTING

12 experimental trial triplets + 12 filler triplets - not randomized for now    
**order:** some-weak, some-strong, some-exhaustive, some-baseline, four-weak, four-strong, ….. 

### Testing the counterbalancing of the position of the correct image:
an array called trials is console.logged: the elements of this array are keys to the ordering of the correct position of primes, this list is popped when creating triplets so the last element of the array corresponds to the first triplet in the experiment (trials are not randomized for now)

1 —> right, right —> primeOShuffle: [0,1] primeTShuffle: [0,1]  
2 —> right, left —> primeOShuffle: [0,1] primeTShuffle: [1,0]  
3 —> left, right —> primeOShuffle: [1,0], primeTShuffle: [0,1]  
4 —> left, left —> primeOShuffle: [1,0], primeTShuffle: [1,0]  

e.g. trials: 1,2,2,4,1,4,3,3,3,2,4,3  
First triplet(3) will have the correct image on the right the left on prime one and on the right on prime two, second triplet(4) will have the correct image on the left on both primes...