# Symmetric alternatives and semantic uncertainty modulate scalar inference

How to cite the paper: 
```
@inproceedings{WaldonDegen2020b,
  title={Symmetric alternatives and semantic uncertainty modulate scalar inference},
  author={Waldon, Brandon and Judith Degen},
  booktitle={Proceedings of the Annual Meeting of the Cognitive Science Society},
  volume={42},
  year={forthcoming}
}
```

## Experiment scripts 

See the `experiments/` directory. Participants were randomly assigned to one of four between-subjects conditions based on the nature of the alternative prime. `c1` corresponded to the condition with Canonical alternative priming; `c2` had Exhaustive-sublcausal priming; `c3` had Exhaustive-only priming, and `c4` had Exhaustive-clausal priming. To run the experiment script from the corresponding condition, open `experiments/c*/html/experiment.html`. 

## Data 

Anonymized raw data can be found in `data/main/`. Single dataframe (with exclusions) can be found at `data/main/data.csv`. A helpful guide to the coding of this data: 

**"trial_type":** “response” or “filler   
**"primeType”:** expression type, 0 (some), 1 (number), 2 (ad hoc)  
**"primeStrength”:** priming condition, 0 (weak), 1 (strong), 2 (alternative), 3 (baseline)  
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

Lastly, note that the *canonical* alternative condition is named simply `alternative` in the **btwncondition** column.

## Analysis

To run multiple participation, native language, and performance exclusions on the data: open `exclude.r` in RStudio. 

To reproduce graphs and grab simple summary statistics (e.g. average completion time): open `viz.r` in RStudio.

To run the main Bayesian mixed effects logistic regression models reported in paper: run `Rscript select.r [condname]` where `condname` is one of `'alternative'`, `'exh-subclausal'`, `'exh-only'`, or `'exh-clausal'`.

To run the post-hoc Bayesian mixed effects analysis investigating behavior bewteen groups, subsetted by expression type: `Rscript btwncond_select.r [exprname]` where `exprname` is one of `'Some'`, `'Number'`, or `'Ad-hoc'`. 

Be sure to specify new output directories!

Note: even when using a random seed, you may see slightly different results than we see in the paper because of your system architecture and/or your R version and/or your C++ compiler along with compiler optimization settings. Here are the settings we used: 

```
R version 3.5.1
Platform: x86_64-pc-linux-gnu (64-bit) 
gcc (GCC) 4.8.5 20150623 (Red Hat 4.8.5-36)

CXX14 = g++ -std=c++1y
CXX14FLAGS = -O3 -Wno-unused-variable -Wno-unused-function -fPIC
``` 

Results of these analyses are saved in the `brmsresults` directory. 

## Models

Model predictions are saved as `.json` files in `models/` and are reproducible by running `lumodel.wppl` from the command line:

```
webppl lumodel.wppl --require webppl-json
```

Figures are reproducible by opening `models/viz.r` in RStudio.
