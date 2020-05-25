var symlist = ["♦", "♣", "✓", "♠", "♥", "◼", "★", "●", "♩", "▲"];
var symText = ["diamond", "club", "tick", "spade", "heart", "square", "star", "circle", "note", "triangle"];
var symPre = ["Some of the symbols are", "There are four", "There is a", "All of the symbols are", "There are six", "There is a"]
var examplePre = ["Many of the symbols are", ["There is a", "above a"]]

/*
  Specification of how cards look. This is read…
  [sym1, sym2, sym3, total].
  sym1 is the one named in the prompt, sym2 and sym3 are alternatives.
*/

var trialCards = {
  someStrong: [3, 6, 0, 9],
  someWeak: [9, 0, 0, 9],
  someFalse: [0, 9, 0, 9],
  fourStrong: [4, 0, 0, 4],
  fourWeak: [6, 0, 0, 6],
  fourFalse: [2, 0, 0, 2],
  adhocStrong: [1, 0, 0, 1],
  adhocWeak: [1, 1, 0, 2],
  adhocFalse: [0, 1, 1, 2],
  response: [0, 0, 0, 0],
  manyExample: [8, 1, 0, 9],
  manyContrast: [3, 6, 0, 9],
  aboveExample: [1, 1, 0, 2]
}

/*
  We build a dictionary for each trial, containing all the relevant information.
  This can then be stored, or the info can be read off and stored independently.
  The dictionary will have the structure:
    {symbols: [] prime: [], response: [], strength : [], etc…}
*/

/*
 Building the responses …
  We're going to avoid adhoc, which will effectively cut the length of the trial by half, due
  to the cross category matching that happens.
*/

function buildTrials() {
  list = [
    // {symbols: [] prime: [], response: [], strength : [], etc…}
  ]
  for (let n = 0; n < 16; n++) { // number of each, note that we need a decent number (more than on actual participants as chances are random will fail the prime test) to ensure that the R analysis will work out (with a fair degree of certainty)
    for (let t = 0; t < 3; t++) { // response, skipping adhoc
      for (let s = 0; s < 2; s++) { // strength
        for (let p = 0; p < 3; p++) { // prime skipping adhoc
          dict = {};
          p1Split = _.shuffle([0, 1]);
          p2Split = _.shuffle([0, 1]);
          dict["response"] = t;
          dict["strength"] = s;
          dict["prime"] = p;
          dict["filler"] = false;
          dict["primeOneShuffle"] = p1Split;
          dict["primeTwoShuffle"] = p2Split;
          dict["goodPrimeOneChoice"] = p1Split.indexOf(1);
          dict["goodPrimeTwoChoice"] = p2Split.indexOf(1);
          dict["primeOneSymbols"] = symbolTriple();
          dict["primeTwoSymbols"] = symbolTriple();
          dict["responseSymbols"] = symbolTriple();
          list.push(dict);
        }
      }
    }
  }
  return list
}


function NumToText(nums) {
  let output = ""
  let names = ["SOME", "NUM4", "ADH", "ALL", "NUM6"]
  for (let i=0;i<nums.length;i++) {
    output += names[nums[i]]
  }
  return output
}

// function buildTrials() {
//   list = [
//     // {symbols: [] prime: [], response: [], strength : [], etc…}
//   ]
//   for (let n = 0; n < 1; n++) { // number of each
//     for (let t = 0; t < 1; t++) { // response, skipping adhoc
//       for (let s = 0; s < 1; s++) { // strength
//         for (let p = 0; p < 1; p++) { // prime skipping adhoc
//           dict = {};
//           p1Split = _.shuffle([0, 1]);
//           p2Split = _.shuffle([0, 1]);
//           dict["response"] = t;
//           dict["strength"] = s;
//           dict["prime"] = p;
//           dict["filler"] = false;
//           dict["primeOneShuffle"] = p1Split;
//           dict["primeTwoShuffle"] = p2Split;
//           dict["goodPrimeOneChoice"] = p1Split.indexOf(1);
//           dict["goodPrimeTwoChoice"] = p2Split.indexOf(1);
//           dict["primeOneSymbols"] = symbolTriple();
//           dict["primeTwoSymbols"] = symbolTriple();
//           dict["responseSymbols"] = symbolTriple();
//           list.push(dict);
//         }
//       }
//     }
//   }
//   return list
// }


/*
  Adding filler trials
  To my understanding we only change the 'response' card when doing filler trials,
  and there are three different possible filler cards. So, we'll get a total of
  6 different filler trials for each category, and from these we need to go down to
  4. We'll use the same method of generation, then split the list into respective
  enrichment categories, and randomly pick 4.
  We adjust the prime some that the response always matches, as we're not interested in
  crossing over prime/filler categories.
*/

function buildFillers() {
  list = []
  for (let n = 1; n <= 1; n++) { // number of each
    for (let t = 3; t < 5; t++) { // response, skipping adhoc
      for (let s = 0; s < 2; s++) { // strength
        // for (let p = 0; p < 3; p++) { // prime
        for (let f = 3; f < 6; f++) { // filler
          dict = {};
          p1Split = _.shuffle([0, 1]);
          p2Split = _.shuffle([0, 1]);
          dict["response"] = t;
          dict["strength"] = s;
          dict["prime"] = t;
          dict["filler"] = f;
          dict["primeOneShuffle"] = p1Split;
          dict["primeTwoShuffle"] = p2Split;
          dict["goodPrimeOneChoice"] = p1Split.indexOf(1);
          dict["goodPrimeTwoChoice"] = p2Split.indexOf(1);
          dict["primeOneSymbols"] = symbolTriple();
          dict["primeTwoSymbols"] = symbolTriple();
          dict["responseSymbols"] = symbolTriple();
          list.push(dict);
        }
      }
    }
    // }
  }
  return list
}


trialList = buildTrials()
fillerList = buildFillers()



/* We now build the filler list, as we're dropping adhoc, we cannot keep the same ratio between response
   and filler trials.
   We had 4*18 = 72 different trials before, and 12 fillers, so a ratio of one filler to every six responses.
   Now we have 4*8 = 32 different trials, so as 32/6 = 5.3, we'd need 5.3… fillers total. We round down to 5.
*/
// someFiller = fillerList.slice(0, 6)
// someFiller = someFiller.concat(_.sample(fillerList.slice(0, 6), 2))
// fourFiller = fillerList.slice(6, 12)
// fourFiller = fourFiller.concat(_.sample(fillerList.slice(6, 12), 2))

someFiller = _.sample(fillerList.slice(0, 6), 5)
fourFiller = _.sample(fillerList.slice(6, 12), 5)
// adhocFiller = _.sample(fillerList.slice(12, 18), 4)

fillerList = someFiller.concat(fourFiller) //.concat(adhocFiller)

// console.log('filler list')
// console.log(fillerList)

trialList = trialList.concat(fillerList)
// trialList = fillerList
/* We've now got an array of trial dictionaries.
   The next thing to do is shuffle these. This is primarily so that
   there is something that the html can access when creating cards.
*/

trialList = _.shuffle(trialList) // randomise order of trials



function makeExamples() {
  list = []

  for (let e = 0; e < 2; e++) {
    dict = {}
    dict["example"] = e
    dict["exampleSymbols"] = symbolTriple()
    list.push(dict);
  }
  return list
}

exampleList = makeExamples()

console.log("Example list: " + exampleList);
console.log("Trial list: " + trialList);



function makeCard(canvasid = 'canvas',
  cardspec,
  symTrip = [0, 1, 2]
) {

  // basic canvas stuff
  var canvas = document.getElementById(canvasid);
  ctx = canvas.getContext("2d");
  ctx.strokeRect(0, 0, canvas.width, canvas.height);

  W = 300;
  H = 1.2 * W;
  canvas.width = W;
  canvas.height = H;

  if (cardspec[3] == 0) {
    x = W / 2
    y = H / 2
    betterText = 'Better picture?'
    ctx.font = "46px serif";
    ctx.fillStyle = "black";
    textDimensions = ctx.measureText(betterText);
    x = (x - (textDimensions.width / 2))
    ctx.fillText(betterText, x, y);
  } else {
    total = cardspec[3]
    sym1 = symTrip[0]
    sym2 = symTrip[1]
    sym3 = symTrip[2]

    // depending on trial type, reconfigure display symbols
    if (cardspec[0] == 0) {
      sym1 = sym3
    }
    if (cardspec[0] == cardspec[3]) {
      sym2 = sym1
    }

    var rows = (Math.ceil(total / 3));
    var cols = (total / rows);


    var drawlist = [];
    var strList = [];

    if (total == 1) {
      drawlist.push(sym1)
    } else {
      for (i = 1; i <= rows * (cols - 1); i++) {
        drawlist.push(sym1)
      }
      for (j = 1; j <= rows; j++) {
        strList.push(sym2)
      }
    }

    // randomise false placement
    if (Math.random() >= 0.5) {
      drawlist = drawlist.concat(strList)
    } else {
      drawlist = strList.concat(drawlist)
    }

    if (total == 9 && cardspec[0] == 8) { // special case for many example
      for (let i = 0; i < 9; i++) {
        drawlist[i] = sym1
      }
      drawlist[0] = sym2
    }
    if (total == 9 && cardspec[0] == 3) { // special case for many example
      for (let i = 0; i < 6; i++) {
        drawlist[i] = sym2
      }
      for (let j = 6; j < 9; j++) {
        drawlist[j] = sym1
      }
    }

    if (rows > 1) {
      [rows, cols] = [cols, rows]
    }

    var symbol = [];
    var Wc = W / cols,
      Hr = H / rows

    var symCount = 0;
    for (i = 1; i <= rows; i++) {
      for (j = 1; j <= cols; j++) {
        symbol[i] = {
          y: (Math.floor((i * Hr) - Hr / 2)),
          x: (Math.floor((j * Wc) - Wc / 2)),
          color: "black",
          unisym: drawlist[symCount],
          draw: symbols,
        };
        symCount++;
        var s = symbol[i];
        s.draw(s.x, s.y, s.color, s.unisym);
      }
    }
  }

  function symbols(x, y, color, unisym) {
    if (unisym == "●") {
      ctx.font = "76px serif"
    } else {
      ctx.font = "46px serif"
    }
    ctx.fillStyle = color;
    textDimensions = ctx.measureText(unisym);
    x = (x - (textDimensions.width / 2))
    if (unisym == "●") {
      y = (y - (-4 * (76 / 46)))
    }
    ctx.fillText(unisym, x, y);
  }
}



/* map list of symbol indices to unicode characters */
function symIndexTripleToUnicode(triple) {
  return triple.map(x => symlist[x])
}



/* map list of symbol indices to text description */
function symIndexTripleToText(triple) {
  return triple.map(x => symText[x])
}



function symbolTriple() {
  let indicies = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  return _.sample(indicies, 3)
}



/*
  To specify the cards we have what amounts to a lookup table.
*/
function specifyTrialCards(trialDict) {

  primeCat = trialDict["prime"]
  responseCat = trialDict["response"]
  strength = trialDict["strength"]
  primeOne = trialDict["primeOneShuffle"].slice(0) // deep copy as modifying
  primeTwo = trialDict["primeTwoShuffle"].slice(0) // again
  primeOneSymbols = symIndexTripleToUnicode(trialDict["primeOneSymbols"])
  primeTwoSymbols = symIndexTripleToUnicode(trialDict["primeTwoSymbols"])
  responseSymbols = symIndexTripleToUnicode(trialDict["responseSymbols"])

  fillerType = trialDict["filler"]

  someStrong = trialCards["someStrong"];
  someWeak = trialCards["someWeak"];
  someFalse = trialCards["someFalse"];
  fourStrong = trialCards["fourStrong"];
  fourWeak = trialCards["fourWeak"];
  fourFalse = trialCards["fourFalse"];
  adhocStrong = trialCards["adhocStrong"];
  adhocWeak = trialCards["adhocWeak"];
  adhocFalse = trialCards["adhocFalse"];
  fillerAllWeak = trialCards["fillerAllWeak"];
  fillerAllStrong = trialCards["fillerAllStrong"];
  fillerSixWeak = trialCards["fillerSixWeak"];
  fillerSixFalse = trialCards["fillerSixFalse"];
  fillerDoubleWeak = trialCards["fillerDoubleWeak"];
  fillerDoubleFalse = trialCards["fillerDoubleFalse"];

  responseCard = trialCards["response"];

  if (strength == 0) { // if weak
    if (primeCat == 0 || primeCat == 3) { // disjunctions to take care of filler trails.
      primeOne[primeOne.indexOf(0)] = someFalse
      primeOne[primeOne.indexOf(1)] = someWeak
      primeTwo[primeTwo.indexOf(0)] = someFalse
      primeTwo[primeTwo.indexOf(1)] = someWeak
    } else if (primeCat == 1 || primeCat == 4) {
      primeOne[primeOne.indexOf(0)] = fourFalse
      primeOne[primeOne.indexOf(1)] = fourWeak
      primeTwo[primeTwo.indexOf(0)] = fourFalse
      primeTwo[primeTwo.indexOf(1)] = fourWeak
    } else if (primeCat == 2 || primeCat == 5) {
      primeOne[primeOne.indexOf(0)] = adhocFalse
      primeOne[primeOne.indexOf(1)] = adhocWeak
      primeTwo[primeTwo.indexOf(0)] = adhocFalse
      primeTwo[primeTwo.indexOf(1)] = adhocWeak
    }
  } else if (strength == 1) { // if strong
    if (primeCat == 0 || primeCat == 3) {
      primeOne[primeOne.indexOf(0)] = someWeak
      primeOne[primeOne.indexOf(1)] = someStrong
      primeTwo[primeTwo.indexOf(0)] = someWeak
      primeTwo[primeTwo.indexOf(1)] = someStrong
    } else if (primeCat == 1 || primeCat == 4) {
      primeOne[primeOne.indexOf(0)] = fourWeak
      primeOne[primeOne.indexOf(1)] = fourStrong
      primeTwo[primeTwo.indexOf(0)] = fourWeak
      primeTwo[primeTwo.indexOf(1)] = fourStrong
    } else if (primeCat == 2 || primeCat == 5) {
      primeOne[primeOne.indexOf(0)] = adhocWeak
      primeOne[primeOne.indexOf(1)] = adhocStrong
      primeTwo[primeTwo.indexOf(0)] = adhocWeak
      primeTwo[primeTwo.indexOf(1)] = adhocStrong
    }
  } else {
    console.log('oh no!')
  }

  if (responseCat == 0) { // someStrong
    responseL = someWeak;
    responseR = responseCard;
  } else if (responseCat == 1) { //
    responseL = fourWeak;
    responseR = responseCard;
  } else if (responseCat == 2) { //
    responseL = adhocWeak;
    responseR = responseCard;
  } else if (responseCat == 3) { // filler
    if (fillerType == 3) {
      responseL = someFalse;
      responseR = responseCard;
    } else if (fillerType == 4) {
      responseL = someWeak;
      responseR = responseCard;
    } else if (fillerType == 5) {
      responseL = someWeak;
      responseR = someStrong;
    }
  } else if (responseCat == 4) { // filler
    if (fillerType == 3) {
      responseL = fourFalse;
      responseR = responseCard;
    } else if (fillerType == 4) {
      responseL = fourWeak;
      responseR = responseCard;
    } else if (fillerType == 5) {
      responseL = fourWeak;
      responseR = fourStrong;
    }
  } else if (responseCat == 5) { // filler
    if (fillerType == 3) {
      responseL = adhocFalse;
      responseR = responseCard;
    } else if (fillerType == 4) {
      responseL = adhocWeak;
      responseR = responseCard;
    } else if (fillerType == 5) {
      responseL = adhocWeak;
      responseR = adhocStrong;
    }
  }

  /* … and gen the cards */
  makeCard(canvasid = 'primeOneL', primeOne[0], primeOneSymbols)
  makeCard(canvasid = 'primeOneR', primeOne[1], primeOneSymbols)
  makeCard(canvasid = 'primeTwoL', primeTwo[0], primeTwoSymbols)
  makeCard(canvasid = 'primeTwoR', primeTwo[1], primeTwoSymbols)
  makeCard(canvasid = 'responseL', responseL, responseSymbols)
  makeCard(canvasid = 'responseR', responseR, responseSymbols)
}



function specifyExampleCards(trialDict) {

  exampleCat = trialDict["example"]
  exampleSymbols = symIndexTripleToUnicode(trialDict["exampleSymbols"])

  manyExample = trialCards["manyExample"];
  aboveExample = trialCards["aboveExample"];
  manyContrast = trialCards["manyContrast"];
  responseCard = trialCards["response"];

  if (exampleCat == 0) { //
    exampleCardL = manyExample
    exampleCardR = manyContrast
  } else if (exampleCat == 1) { //
    exampleCardL = aboveExample
    exampleCardR = responseCard
  } else {
    console.log('error')
  }
  makeCard(canvasid = 'exampleL', exampleCardL, exampleSymbols);
  makeCard(canvasid = 'exampleR', exampleCardR, exampleSymbols);
}



function conditionSentence(condition, symbols) {
  if (condition != 5) {
    condText = "" + symPre[condition] + " " + symText[symbols[0]]
    if (condition != 2) {
      condText += "s"
    }
  } else {
    condText = "" + symPre[condition] + " " + symText[symbols[0]] + " and a " + symText[symbols[1]]
  }
  return condText
}



function exampleSentence(condition, symbols) {
  if (condition == 0) {
    condText = examplePre[condition] + " " + symText[symbols[0]] + "s"
  } else if (condition == 1) {
    condText = "" + examplePre[condition][0] + " " + symText[symbols[0]] + " " + examplePre[condition][1] + " " + symText[symbols[1]]
  }
  return condText
}


var testKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#test").html('The keyboard button you pressed will take you to the next slide.')
      break;
    case "ArrowLeft":
      $("#test").html('The keyboard button you pressed will select the left card.')
      break;
    case "ArrowRight":
      $("#test").html('The keyboard button you pressed will select the right card.')
      break;
    default:
      $("#test").html('The keyboard button you pressed does not have a function in this experiment.')
  }
  event.preventDefault();
}

function addTestKeys() {
  window.addEventListener("keydown", testKeys);
}

function clearTestKeys() {
  window.removeEventListener("keydown", testKeys);
}

var i0Keys = function(event) {

  if (event.defaultPrevented) {
    return; // Do nothing if the event was already processed
  }
  switch (event.key) {
    case " ":
      $("#start_button").click()
      break;
    default:
      return; // Quit when this doesn't handle the key event.
  }
  event.preventDefault();
}


var consentKeys = function(event) {

  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#consentButton").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}

var instructionKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#instructionButton").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}

var exampleKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#continueButton").click()
      break;
    case "ArrowLeft":
      $("#exampleChoiceL").click()
      break;
    case "ArrowRight":
      $("#exampleChoiceR").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}


var beginExperimentKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#beginButton").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}

var primeOneKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#continueButton").click()
      break;
    case "ArrowLeft":
      $("#primeOneChoiceL").click()
      break;
    case "ArrowRight":
      $("#primeOneChoiceR").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}

var primeTwoKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#continueButton").click()
      break;
    case "ArrowLeft":
      $("#primeTwoChoiceL").click()
      break;
    case "ArrowRight":
      $("#primeTwoChoiceR").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}

var responseKeys = function(event) {
  if (event.defaultPrevented) {
    return;
  }
  switch (event.key) {
    case " ":
      $("#continueButton").click()
      break;
    case "ArrowLeft":
      $("#responseChoiceL").click()
      break;
    case "ArrowRight":
      $("#responseChoiceR").click()
      break;
    default:
      return;
  }
  event.preventDefault();
}


function addi0Keys() {
  window.addEventListener("keydown", i0Keys);
}

function cleari0Keys() {
  window.removeEventListener("keydown", i0Keys);
}

function addConsentKeys() {
  window.addEventListener("keydown", consentKeys);
}

function clearConsentKeys() {
  window.removeEventListener("keydown", consentKeys);
}

function addInstructionKeys() {
  window.addEventListener("keydown", instructionKeys);
}

function clearInstructionKeys() {
  window.removeEventListener("keydown", instructionKeys);
}

function addExampleKeys() {
  window.addEventListener("keydown", exampleKeys);
}

function clearExampleKeys() {
  window.removeEventListener("keydown", exampleKeys);
}

function addBeginExperimentKeys() {
  window.addEventListener("keydown", beginExperimentKeys);
}

function clearBeginExperimentKeys() {
  window.removeEventListener("keydown", beginExperimentKeys);
}

function addPrimeOneKeys() {
  window.addEventListener("keydown", primeOneKeys);
}

function clearPrimeOneKeys() {
  window.removeEventListener("keydown", primeOneKeys);
}

function addPrimeTwoKeys() {
  window.addEventListener("keydown", primeTwoKeys);
}

function clearPrimeTwoKeys() {
  window.removeEventListener("keydown", primeTwoKeys);
}

function addResponseKeys() {
  window.addEventListener("keydown", responseKeys);
}

function clearResponseKeys() {
  window.removeEventListener("keydown", responseKeys);
}

function clearTrialKeys() {
  window.removeEventListener("keydown", primeOneKeys);
  window.removeEventListener("keydown", primeTwoKeys);
  window.removeEventListener("keydown", responseKeys);
}