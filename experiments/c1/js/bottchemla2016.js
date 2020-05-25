function make_slides(f) {

  var slides = {};

  slides.bot = slide({
    name : "bot",
    start: function() {
      $('.err1').hide();
      $('.err2').hide();
      $('.disq').hide();
      exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
      exp.lives = 0;
      var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
      var question = 'Who does ' + exp.speaker + ' talk to?';
      document.getElementById("s").innerHTML = story;
      document.getElementById("q").innerHTML = question;
    },
    button : function() {
      exp.text_input = document.getElementById("text_box").value;
      var lower = exp.listener.toLowerCase();
      var upper = exp.listener.toUpperCase();

      if ((exp.lives < 3) && ((exp.text_input == exp.listener)|(exp.text_input == lower) | (exp.text_input== upper))){
        exp.data_trials.push({
          "slide_number": exp.phase,
          "slide_type" : "bot_check",
          "image" : exp.listener,
          "audio" : "",
          "response" : [0,exp.text_input]
        });
        exp.go();
      }
      else {
        exp.data_trials.push({
          "slide_number": exp.phase,
          "slide_type" : "bot_check",
          "image" : exp.listener,
          "audio" : "",
          "response" : [0,exp.text_input]
        });
        if (exp.lives == 0){
          $('.err1').show();
        }if (exp.lives == 1){
          $('.err1').hide();
          $('.err2').show();
        }if (exp.lives == 2){
          $('.err2').hide();
          $('.disq').show();
          $('.button').hide();
        }
        exp.lives++;
      } 
    },
  });

  slides.i0 = slide({
    name: "i0",
    start: function() {
      exp.startT = Date.now();
      addi0Keys();
    }
  });

  // slides.consent = slide({
  //   name: "consent",
  //   start: function() {
  //     exp.startT = Date.now();
  //     $("#consent_2").hide();
  //     exp.consent_position = 0;

  //     cleari0Keys();
  //     addConsentKeys();
  //   },
  //   button: function() {
  //     if (exp.consent_position == 0) {
  //       exp.consent_position++;
  //       $("#consent_1").hide();
  //       $("#consent_2").show();
  //     } else {
  //       exp.go(); //use exp.go() if and only if there is no "present" data.
  //     }
  //   }
  // });

  /*Consult the code in the consent slide if you
    want to break down very long instructions */
  slides.instructions = slide({
    name: "instructions",
    start: function() {
      cleari0Keys();
      //clearConsentKeys();
      addInstructionKeys();
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


  slides.keyboard = slide({
    name: "keyboard",
    start: function() {
      clearInstructionKeys();
      addTestKeys();
    },
    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.example = slide({
    name: "example",
    start: function() {
      clearTestKeys();
      addExampleKeys();

      $("#exampleStatus").html("Here is the first example to help familiarise you with the task.");
    },

    present: exampleList,

    present_handle: function(stim) {

      this.stim = stim


      $(".err").hide();
      $('input[name=exampleChoice]:checked').prop('checked', false);
      $("#exampleCondition").html(exampleSentence(this.stim["example"], this.stim["exampleSymbols"]));

      specifyExampleCards(this.stim); // use trial dictionary build cards.

      slide.condition = 0;
    },

    cardButton: function() { // if a card is selected
      $('.err').hide(); // hide error
      slide.condition = 1; // note that something is selected
    },

    button: function() {
      if (slide.condition == 1) {
        if (this.stim["example"] == $('input[name=exampleChoice]:checked').val()) { // make sure correct response on examples
          $("#exampleStatus").html("Here is the second example to help familiarise you with the task. Remember, the 'Better Picture?' picture should be selected if you do not feel that the other picture sufficiently captures the sentence meaning.");
          this.log_responses(); // log responses
          _stream.apply(this); // store data}
        } else {
          $("#exampleErr").html("Are you sure? Please consider which card best matches the sentence.")
          $('.err').show();
        }
      } else {
        $("#exampleErr").html("Please select one of the options.")
        $('.err').show();
      }
    },

    log_responses: function() {

      // exp.data_trials.push({ // data to be stored
      //   "trial_type": "example",
      // });
      // probably shouldn't log this unless it's going to be exactly the same format
    },
  });

  slides.begin = slide({

    name: "begin",
    start: function() {
      clearExampleKeys();
      addBeginExperimentKeys();
    },

    button: function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  slides.trial = slide({ // the main slide

    name: "trial",
    start: function() {
      trialMode = true
      clearBeginExperimentKeys();
    },

    present: trialList, //trialOrder,

    //this gets run only at the beginning of the block, which means for each 'present' thing.
    present_handle: function(stim) {

      this.block_start = Date.now(); 
      this.stim = stim; //

      console.log(stim);

      specifyTrialCards(this.stim); // use trial dictionary build cards.

      // Uncheck card buttons and erase the previous values
      this.stim.criticalResponse = null;
      this.stim.primeOneChoice = null;
      this.stim.primeTwoChoice = null;
      this.stim.responseChoice = null;

      $('input[name=primeOneChoice]:checked').prop('checked', false);
      $('input[name=primeTwoChoice]:checked').prop('checked', false);
      $('input[name=responseChoice]:checked').prop('checked', false);
      slide.condition = 0; // no card has been selected

      // hide everything but for the primeOne cards
      $(".err").hide();
      $(".responseContainerL").hide();
      $(".responseContainerR").hide();
      clearResponseKeys();
      $(".primeTwoContainerL").hide();
      $(".primeTwoContainerR").hide();
      $(".primeOneContainerL").show();

      $(".primeOneContainerR").show();
      addPrimeOneKeys();
      rpPrimeOne = true
      rpResponse = false

      // show primeOne stimulus
      $("#trialCondition").html(conditionSentence(this.stim["prime"], this.stim["strength"],this.stim["primeOneSymbols"], 0));
      this.stim.primeOSentence = conditionSentence(this.stim["prime"], this.stim["strength"],this.stim["primeOneSymbols"], 0);
      this.stim = stim; // I like to store this information in the slide so I can record it later.
    },

    cardButton: function() { // if a card is selected
      $('.err').hide(); // hide error
      slide.condition = 1; // note that something is selected
    },

    /*
      The button function does a lot.
      Primarily, it updates the slide to follow the trial.
      We do this by first showing relevant info for the first prime, and then if a selection has
      been made we update to the second, and then to the response, when we store information and then
      move to the next trial.
      The function does this by checking in reverse, as response takes precedence over primeTwo, etc…
    */

    button: function() { // what to do when the lower button is clicked
      if (slide.condition == 1) { // so long as a button is clicked, we update…
        $(".err").hide(); // hide error
        this.stim.responseChoice = $('input[name=responseChoice]:checked').val(); // now update one button values
        this.stim.primeOneChoice = $('input[name=primeOneChoice]:checked').val();
        this.stim.primeTwoChoice = $('input[name=primeTwoChoice]:checked').val();
        if (this.stim.responseChoice != null) { // if one has chosen the response
          this.log_responses(); // log responses
          _stream.apply(this); // store data
        } else if (this.stim.primeTwoChoice != null) { // if one has chosen the second prime…
          $("#trialCondition").html(conditionSentence(this.stim["response"], this.stim["strength"], this.stim["responseSymbols"], 2));
          this.stim.responseSentence = conditionSentence(this.stim["response"], this.stim["strength"],this.stim["responseSymbols"], 2);
          $(".primeTwoContainerL").hide();
          $(".primeTwoContainerR").hide();
          clearPrimeTwoKeys();
          $(".responseContainerL").show();
          $(".responseContainerR").show();
          rpResponse = true
          rpPrimeTwo = false
          addResponseKeys();
          slide.condition = 0;
          this.response_start = Date.now(); // start gauging reaction time at beginning of response trial 
        } else if (this.stim.primeOneChoice != null) { // if one has chosen the first prime…
          $("#trialCondition").html(conditionSentence(this.stim["prime"], this.stim["strength"], this.stim["primeTwoSymbols"], 1));
          this.stim.primeTSentence = conditionSentence(this.stim["prime"], this.stim["strength"],this.stim["primeTwoSymbols"], 1)
          $(".primeOneContainerL").hide();
          $(".primeOneContainerR").hide();
          clearPrimeOneKeys();
          $(".primeTwoContainerL").show();
          $(".primeTwoContainerR").show();
          rpPrimeTwo = true
          rpPrimeOne = false
          addPrimeTwoKeys();
          slide.condition = 0;
        }
      } else { // otherwise…
        $('.err').show() // show error
      }
    },

    log_responses: function() {
      exp.data_trials.push({ // data to be stored
        "trial_type": (this.stim["filler"] == false) ? "response" : "filler",
        "primeType": this.stim["prime"],
        //"primeTypeText": NumToText([this.stim["prime"]]),
        //"responseType": this.stim["response"],
        //"responseTypeText": NumToText([this.stim["response"]]),
        "primeStrength": this.stim["strength"],
        //"primeStrengthText": (this.stim["strength"] == 0) ? "weak" : "strong",
        //"primeStrengthText": if (this.stim["strength"] == 0) {"weak"; } if (this.stim["strength"] == 1) {"strong";} if (this.stim["strength"] == 2) {"alternative";} if (this.stim["strength"] == 3) {"baseline";} if (this.stim["strength"] == 4) {"filler";},
        "pOSentence": this.stim.primeOSentence,
        "pOSymbols": this.stim.primeOneSymbols,
        "pOwrongAnswer": this.stim.randomA,
        "pOChoice": this.stim.primeOneChoice,
        "goodPOChoice": this.stim["goodPrimeOneChoice"],
        "primeOShuffle": this.stim["primeOneShuffle"],
        "correctPOChoice": (this.stim.primeOneChoice == this.stim["goodPrimeOneChoice"]),
        "pTSentence": this.stim.primeTSentence,
        "pTSymbols": this.stim.primeTwoSymbols,
        "pTwrongAnswer": this.stim.randomB,
        "pTChoice": this.stim.primeTwoChoice,
        "goodPTChoice": this.stim["goodPrimeTwoChoice"],
        "primeTShuffle": this.stim["primeTwoShuffle"],
        "correctPTChoice": (this.stim.primeTwoChoice == this.stim["goodPrimeTwoChoice"]),
        "correctPChoices": ((this.stim.primeOneChoice == this.stim["goodPrimeOneChoice"] == true) && (this.stim.primeTwoChoice == this.stim["goodPrimeTwoChoice"]) == true),
        //"responseCatMatchesPrimeCat": ((this.stim["prime"] == this.stim["response"]) ? (this.stim["prime"] == this.stim["response"]) : "false"),
        //"WithBet": ((this.stim["prime"] == this.stim["response"]) ? "within" : "between"), // within/between category
        //"WithCat": NumToText([this.stim["prime"]]),

        //"WithBetN": ((this.stim["prime"] == this.stim["response"]) ? 1 : 0), // within/between category
        //"WithCatN": this.stim["prime"], // this returns the category, assuming everything is the same
        //"BetCat": NumToText([this.stim["prime"], this.stim["response"]]),
        //"BetCatN": (Math.pow(2, this.stim["prime"] + 1) * Math.pow(3, this.stim["response"] + 1)), // use godel encoding to get unique number for cross cat trials
        "responseSentence": this.stim.responseSentence,
        "responseSymbols": this.stim.responseSymbols,
        "responseChoice": this.stim.responseChoice, // rename response
        //"responseChoiceText": (this.stim.responseChoice == 0) ? "weak" : "better",
        "block_rt": Date.now() - _s.block_start,
        "response_rt": Date.now() - _s.response_start,
        "btwncondition" : "alternative",
        // "primeOneSymbols": this.stim["primeOneSymbols"],
        // "primeTwoSymbols": this.stim["primeTwoSymbols"],
        // "responseSymbols": this.stim["responseSymbols"],
      });
    }
  });



  slides.subj_info = slide({
    start: function() {
      trialMode = false
      clearTrialKeys();
    },
    name: "subj_info",
    submit: function(e) {
      exp.subj_data = {
        language: $("#language").val(),
        enjoyment: $("#enjoyment").val(),
        asses: $('input[name="assess"]:checked').val(),
        age: $("#age").val(),
        gender: $("#gender").val(),
        education: $("#education").val(),
        comments: $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });



  slides.thanks = slide({
    name: "thanks",
    start: function() {
      exp.data = {
        "trials": exp.data_trials,
        "catch_trials": exp.catch_trials,
        "system": exp.system,
        "condition": exp.condition,
        "subject_information": exp.subj_data,
        "time_in_minutes": (Date.now() - exp.startT) / 60000
      };
      setTimeout(function() {
        turk.submit(exp.data);
      }, 1000);
      $("#csv").html(JSON.stringify(exp.data))
    }
  });

  return slides;
}

/// init ///
function init() {
  //blocks of the experiment:
  exp.structure = ["bot", "i0", "instructions", "keyboard", "example", "begin", "trial", "subj_info", "thanks"];

  // generally no need to change anything below
  exp.trials = [];
  exp.catch_trials = [];
  exp.data_trials = [];
  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width
  };

  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {
        $("#mustaccept").show();
      });
      exp.go();
    }
  });

  exp.go(); //show first slide
}