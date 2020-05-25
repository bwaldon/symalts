/*
  Below is a function which simulates a participant going through the experiment and choosing cards at random.
  It's very simple, but gets the job done, and the idea is that this can be used a) for testing, and b) to see how the results of actual participants compare to what we get with random participants (which seems like it could be interesting as who knows how this may play out in practice).
*/

function  () {
  $("#start_button").click()
  $("#consentButton").click()
  $("#consentButton").click()
  $("#instructionButton").click()
  $("#instructionButton").click()
  $("#exampleChoiceL").click()
  $("#continueButton").click()
  $("#exampleChoiceR").click()
  $("#continueButton").click()
  $("#continueButton").click()
  while (trialMode == true) {
    cardChoice = _.sample([0,1], 1)
    if (cardChoice == 0) {
      if (rpPrimeOne == true) {
        $("#primeOneChoiceL").click();
      } else if (rpPrimeTwo == true) {
        $("#primeTwoChoiceL").click();
      } else if (rpResponse == true) {
        $("#responseChoiceL").click();
      }
    } else if (cardChoice == 1) {
      if (rpPrimeOne == true) {
        $("#primeOneChoiceR").click();
      } else if (rpPrimeTwo == true) {
        $("#primeTwoChoiceR").click();
      } else if (rpResponse == true) {
        $("#responseChoiceR").click();
      }
    }
    $("#continueButton").click()
  }
  $("#submitButton").click()
}