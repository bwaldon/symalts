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
  someStrong: [6, 3, 0, 9],
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
  aboveExample: [1, 1, 0, 2],
  mathAnswer: [1, 0, 0, 1],
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
  var correctTPosition = _.shuffle([1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4])// 12 elements total
  //console.log("trials: " + correctTPosition); 

  for (let n = 0; n < 4; n++) { // number of each
    for (let t = 0; t < 3; t++) { // response (expression): 0(some), 1(four), 2(adhoc)
      for (let s = 0; s < 4; s++) { // strength: 0(weak), 1(strong), 2(alternative), 3(baseline)
          dict = {};

          var a = correctTPosition.pop();
          if (a == 1){
            p1Split = [0, 1];
            p2Split = [0, 1];
          } else if (a == 2){
            p1Split = [0, 1];
            p2Split = [1, 0];
          } else if (a == 3){
            p1Split = [1, 0];
            p2Split = [0, 1];
          } else if (a == 4){
            p1Split = [1, 0];
            p2Split = [1, 0];
          } 
          dict["response"] = t;
          dict["strength"] = s;
          dict["prime"] = t;
          dict["filler"] = false;
          dict["primeOneShuffle"] = p1Split;                                                 // [0,1] o [1,0]
          dict["primeTwoShuffle"] = p2Split;                                                 // [0,1] o [1,0]
          dict["goodPrimeOneChoice"] = p1Split.indexOf(1);                                   // 0 or 1
          dict["goodPrimeTwoChoice"] = p2Split.indexOf(1);
          dict["responseSymbols"] = symbolTriple();
          
          if (dict["strength"] == 3) {
            var x = Math.floor(Math.random() * 10); // integer btw 0 and 9
            var y = Math.floor(Math.random() * 10);
            var z = x + y;
            var a = Math.floor(Math.random() * 10);
            var b = Math.floor(Math.random() * 10);
            var c = a + b;
            dict["primeOneSymbols"] = [z, x, y];   
            dict["primeTwoSymbols"] = [c, a, b];
            dict["randomA"] = [Math.floor(Math.random() * 10),0,0];
            if(dict["randomA"][0] == z) {
              dict["randomA"][0] = z + 1
            }
            dict["randomB"] = [Math.floor(Math.random() * 10),0,0];
            if(dict["randomB"][0] == c) {
              dict["randomB"][0] = c + 1
            }
          }
          else {
            dict["primeOneSymbols"] = symbolTriple();
            dict["primeTwoSymbols"] = symbolTriple();
            dict["randomA"] = [0,0,0];
            dict["randomB"] = [0,0,0];
          }
          list.push(dict);
        //}
      }
    }
  }
  return list
}

/* Function to turn card number into text description. */
function NumToText(nums) {
  let output = ""
  let names = ["SOME", "NUM4", "ADH", "ALL", "NUM6"]
  for (let i=0;i<nums.length;i++) {
    output += names[nums[i]]
  }
  return output
}

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
  var correctFPosition = _.shuffle([1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4])// 12 elements total
  //console.log("fillers: " + correctFPosition); 
  
  for (let n = 0; n < 4; n++) { // number of each
    for (let t = 0; t < 3; t++) { // response (expression): 0(some), 1(four), 2(adhoc)
      for (let s = 0; s < 4; s++) { // strength: 0(weak), 1(strong), 2(alternative), 3(baseline)
        //for (let f = 3; f < 6; f++) { // filler (switched from prime)
          dict = {};

          var a = correctFPosition.pop();
          if (a == 1){
            p1Split = [0, 1];
            p2Split = [0, 1];
          } else if (a == 2){
            p1Split = [0, 1];
            p2Split = [1, 0];
          } else if (a == 3){
            p1Split = [1, 0];
            p2Split = [0, 1];
          } else if (a == 4){
            p1Split = [1, 0];
            p2Split = [1, 0];
          }           
          dict["response"] = t;
          dict["strength"] = s;
          dict["prime"] = t;
          dict["filler"] = true;
          dict["primeOneShuffle"] = p1Split;
          dict["primeTwoShuffle"] = p2Split;
          dict["goodPrimeOneChoice"] = p1Split.indexOf(1);
          dict["goodPrimeTwoChoice"] = p2Split.indexOf(1);
          dict["responseSymbols"] = symbolTriple();
          if (dict["strength"] == 3) {
            var x = Math.floor(Math.random() * 10); // integer btw 0 and 9
            var y = Math.floor(Math.random() * 10);
            var z = x + y;
            var a = Math.floor(Math.random() * 10);
            var b = Math.floor(Math.random() * 10);
            var c = a + b;
            dict["primeOneSymbols"] = [z, x, y];   
            dict["primeTwoSymbols"] = [c, a, b];
            dict["randomA"] = [Math.floor(Math.random() * 10),0,0];
            if(dict["randomA"] == z) {
              dict["randomA"] = z - 1
            }
            dict["randomB"] = [Math.floor(Math.random() * 10),0,0];
            if(dict["randomB"] == c) {
              dict["randomB"] = c -1
            }
          }
          else {
            dict["primeOneSymbols"] = symbolTriple();
            dict["primeTwoSymbols"] = symbolTriple();
            dict["randomA"] = [0,0,0];
            dict["randomB"] = [0,0,0];
          }
          list.push(dict);
        //}
      }
    }
  }
  return list
}

trialList = buildTrials()
fillerList = buildFillers()


trialList = trialList.concat(fillerList)
trialList = _.shuffle(trialList)

/* We've now got an array of trial dictionaries.
   The next thing to do is shuffle these. This is primarily so that
   there is something that the html can access when creating cards.
*/

 // randomise order of trials

length = trialList.length;
console.log("length: " + length);


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


/////
//primeOne[primeOne.indexOf(0)] = someStrong
//primeOne[primeOne.indexOf(1)] = someWeak

//makeCard(canvasid = 'primeOneL', primeOne[0], primeOneSymbols)
/////

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
    if (cardspec[1] == cardspec[3]) {
      sym1 = sym3 = sym2
    } else {
      if (cardspec[0] == 0) {
        sym1 = sym3
      }
      if (cardspec[0] == cardspec[3]) {
        sym2 = sym1
      }
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
  primeOne = trialDict["primeOneShuffle"].slice(0) // deep copy as modifying           [0,1] or [1,0]
  primeTwo = trialDict["primeTwoShuffle"].slice(0) // again
  primeOneSymbols = symIndexTripleToUnicode(trialDict["primeOneSymbols"])
  primeTwoSymbols = symIndexTripleToUnicode(trialDict["primeTwoSymbols"])
  responseSymbols = symIndexTripleToUnicode(trialDict["responseSymbols"])
  randomA = trialDict["randomA"]
  randomB = trialDict["randomB"]

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
  mathAnswer = trialCards["mathAnswer"];
  responseCard = trialCards["response"];

  if (strength == 0) { // if weak
    if (primeCat == 0) { // disjunctions to take care of filler trails. // if type == 0 (some)
      primeOne[primeOne.indexOf(0)] = someFalse
      primeOne[primeOne.indexOf(1)] = someWeak
      primeTwo[primeTwo.indexOf(0)] = someFalse
      primeTwo[primeTwo.indexOf(1)] = someWeak
    } else if (primeCat == 1) { // if type == num
      primeOne[primeOne.indexOf(0)] = fourFalse
      primeOne[primeOne.indexOf(1)] = fourWeak
      primeTwo[primeTwo.indexOf(0)] = fourFalse
      primeTwo[primeTwo.indexOf(1)] = fourWeak
    } else if (primeCat == 2) { //if type == adhoc
      primeOne[primeOne.indexOf(0)] = adhocFalse
      primeOne[primeOne.indexOf(1)] = adhocWeak
      primeTwo[primeTwo.indexOf(0)] = adhocFalse
      primeTwo[primeTwo.indexOf(1)] = adhocWeak
    }
  } else if (strength == 1) { // if strong          
    if (primeCat == 0) {
      primeOne[primeOne.indexOf(0)] = someWeak
      primeOne[primeOne.indexOf(1)] = someStrong
      primeTwo[primeTwo.indexOf(0)] = someWeak
      primeTwo[primeTwo.indexOf(1)] = someStrong
    } else if (primeCat == 1) {
      primeOne[primeOne.indexOf(0)] = fourWeak
      primeOne[primeOne.indexOf(1)] = fourStrong
      primeTwo[primeTwo.indexOf(0)] = fourWeak
      primeTwo[primeTwo.indexOf(1)] = fourStrong
    } else if (primeCat == 2) {
      primeOne[primeOne.indexOf(0)] = adhocWeak
      primeOne[primeOne.indexOf(1)] = adhocStrong
      primeTwo[primeTwo.indexOf(0)] = adhocWeak
      primeTwo[primeTwo.indexOf(1)] = adhocStrong
    }
  } else if (strength == 2){ // if alternative
    if (primeCat == 0) {
      primeOne[primeOne.indexOf(0)] = someStrong
      primeOne[primeOne.indexOf(1)] = someWeak
      primeTwo[primeTwo.indexOf(0)] = someStrong
      primeTwo[primeTwo.indexOf(1)] = someWeak
    } else if (primeCat == 1) {
      primeOne[primeOne.indexOf(0)] = fourStrong
      primeOne[primeOne.indexOf(1)] = fourWeak
      primeTwo[primeTwo.indexOf(0)] = fourStrong
      primeTwo[primeTwo.indexOf(1)] = fourWeak
    } else if (primeCat == 2) {
      primeOne[primeOne.indexOf(0)] = adhocStrong
      primeOne[primeOne.indexOf(1)] = adhocWeak
      primeTwo[primeTwo.indexOf(0)] = adhocStrong
      primeTwo[primeTwo.indexOf(1)] = adhocWeak
    }
  } else if (strength == 3) { // baseline
      primeOne[primeOne.indexOf(0)] = mathAnswer
      primeOne[primeOne.indexOf(1)] = mathAnswer
      primeTwo[primeTwo.indexOf(0)] = mathAnswer
      primeTwo[primeTwo.indexOf(1)] = mathAnswer
      primeOneSymbols = trialDict["primeOneSymbols"]
      primeTwoSymbols = trialDict["primeTwoSymbols"]

      var randomOrder = Math.floor(Math.random() * 10);
      //var randomA = [(Math.floor(Math.random() * 10)),0,0];
      //var randomB = [(Math.floor(Math.random() * 10)),0,0];
      if (randomOrder % 2 == 0) {
        makeCard(canvasid = 'primeOneL', primeOne[0], primeOneSymbols)
        makeCard(canvasid = 'primeOneR', primeOne[1], randomA)
        makeCard(canvasid = 'primeTwoL', primeTwo[0], randomB)
        makeCard(canvasid = 'primeTwoR', primeTwo[1], primeTwoSymbols)
      } else {
        makeCard(canvasid = 'primeOneL', primeOne[0], randomA)
        makeCard(canvasid = 'primeOneR', primeOne[1], primeOneSymbols)
        makeCard(canvasid = 'primeTwoL', primeTwo[0], primeTwoSymbols)
        makeCard(canvasid = 'primeTwoR', primeTwo[1], randomB)
      }
   }

  else {
    console.log('oh no!')
  }

  if (fillerType == false) {
    if (responseCat == 0) { // some 
      responseL = someWeak;
      responseR = responseCard;
    } else if (responseCat == 1) { // num
      responseL = fourWeak;
      responseR = responseCard;
    } else if (responseCat == 2) { // adhoc
      responseL = adhocWeak;
      responseR = responseCard;
    } 
  }

  if (fillerType == true) {
    if (responseCat == 0) { // some - someStrong
      responseL = someFalse;
      responseR = responseCard;
    } else if (responseCat == 1) { // num
      responseL = fourFalse;
      responseR = responseCard;
    } else if (responseCat == 2) { // adhoc
      responseL = adhocFalse;
      responseR = responseCard;
    }
  }

  /* … and gen the cards */
  if (strength !== 3) {
    makeCard(canvasid = 'primeOneL', primeOne[0], primeOneSymbols)
    makeCard(canvasid = 'primeOneR', primeOne[1], primeOneSymbols)
    makeCard(canvasid = 'primeTwoL', primeTwo[0], primeTwoSymbols)
    makeCard(canvasid = 'primeTwoR', primeTwo[1], primeTwoSymbols)
  }
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

function conditionSentence(condition, condition_2, symbols, triplet) { // condition --> prime, condition_2 --> strength
  if (condition_2 == 0 | condition_2 == 1 | (condition_2 == 2 && triplet == 2) | (condition_2 == 3 && triplet ==  2) ) { // prime type == weak or strong altresponse
    if (condition == 0 | condition == 1)  // some or number
      condText = "" + symPre[condition] + " " + symText[symbols[0]] + "s"  + "."
    else // adhoc
      condText = "" + symPre[condition] + " " + symText[symbols[0]]  + "."
  }
  if (condition_2 == 2) { // prime_type == alternative
    if (triplet !== 2) {
      if (condition == 0 | condition == 1) // some or number
        condText = "" + symPre[condition+3] + " " + symText[symbols[0]] + "s"  + "."
      if (condition == 2) // adhoc
        condText = "" + symPre[condition+3] + " " + symText[symbols[0]]  + " and a " + symText[symbols[1]]  + "."
    }
  }
  if (condition_2 == 3) { // prime_type == math problem
    if (triplet !== 2) { // prime 1 or prime 2 
      condText = symbols[1] + " + " + symbols[2] + " = ?"
    } 
    // if (condition == 0 && triplet == 2){ // some response
    //   condText = "" + symPre[condition] + " " + symText[symbols[0]] 
    // } if (condition == 1 && triplet == 2){ // four response
    //   condText = "" + symPre[condition] + " " + symText[symbols[0]] + "s"
    // } if (condition == 2 && triplet == 2) { // adhoc response
    //   condText = "" + symPre[condition] + " " + symText[symbols[0]]
    // }
  }
  return condText
}


function exampleSentence(condition, symbols) {
  if (condition == 0) {
    condText = examplePre[condition] + " " + symText[symbols[0]] + "s" + "."
  } else if (condition == 1) {
    condText = "" + examplePre[condition][0] + " " + symText[symbols[0]] + " " + examplePre[condition][1] + " " + symText[symbols[1]] + "."
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