var pragContext;
var contexts = ["good", "bad"],
pragContext = contexts[Math.floor(Math.random() * contexts.length)];
console.log(pragContext);

function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name : "i0",
    start: function() {
    exp.startT = Date.now();
    }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.trial = slide({
    name : "trial",
    present: exp.stims_shuffled, //every element in exp.stims is passed to present_handle one by one as 'stim'
    start: function(){
      exp.counter = 0;

    },
    present_handle : function(stim) {
      exp.selection_array=[];
      exp.time_array=[];
      exp.trial_start = Date.now();
      console.log("time:"+(Date.now()-exp.trial_start))

      $(".err").hide();
      $(".grid-container").show();
    
      this.stim = stim; // store this information in the slide so you can record it later

      //note_cb: increments
      // var instruction = stim.instruction3;
      // words = instruction.split(" ")
      // init_instruction = words[0]+ " " + words[1] + " " + words[2] + " ..."; // click on the
      // instruction1 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5] + " ..."; // click on the boy that has
      // instruction2 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5]+ " " + words[6] + " " + words[7] + " " + words[8] + " ..."; // click on the boy that has two of Susan's
      // instruction3 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + " " + words[5]+ " " + words[6] + " " + words[7] + " " + words[8] + " " + words[9] + ".";  // click on the boy that has two of Susan's pears
      // const instruction_array=[instruction1,instruction2,instruction3]

      //change_cb
      var instruction = stim.instruction;
      words = instruction.split(" ")
      init_instruction = words[0]+ " " + words[1] + " " + words[2] + " ..."; // click on the
      //sentences are either 4 or 5 words long.
      //short case:
      const instruction_array=[]
      if (stim.utterance_type =="unmodified") {
        instruction1 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + "."; // click on the axe.
        instruction_array.push(instruction1)
      } else { //long case
        instruction1 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " ..."; //click on the small...
        instruction_array.push(instruction1)
        instruction2 = words[0]+ " " + words[1] + " " + words[2] + " " + words[3] + " " + words[4] + "." //click on the small cookie.
        instruction_array.push(instruction2)
      }

      $(".instruction").html(init_instruction);
  
      loc_shuffled = _.shuffle([".loc1", ".loc2", ".loc3", ".loc4"]) //shuffles the ordering of the target and competitors between trials
      var loc_target = '<img src="images/'+stim.target_pic+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[0]).html(loc_target);
      if (pragContext === "good") {
        var loc_contrast = '<img src="images/'+stim.target_contrast_good+'" style="width: 90%; height: 90%" class="img-scale-down">';
        $(loc_shuffled[1]).html(loc_contrast);
      } else {
        var loc_contrast = '<img src="images/'+stim.target_contrast_bad+'" style="width: 90%; height: 90%" class="img-scale-down">';
        $(loc_shuffled[1]).html(loc_contrast);
      }
      var loc_big_filler = '<img src="images/'+stim.big_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[2]).html(loc_big_filler);
      var loc_small_filler = '<img src="images/'+stim.small_filler+'" style="width: 90%; height: 90%" class="img-scale-down">';
      $(loc_shuffled[3]).html(loc_small_filler);

      //click listeners
      //when something (event e) is clicked...
      //exp.counter counts clicks
      $(".loc").bind("click",function(e) {
        e.preventDefault();
        if (stim.utterance_type=="unmodified") {
          if (exp.counter>0){
            exp.selection_array.push($(this).data().loc)
            exp.time_array.push(Date.now()-exp.trial_start)
            console.log("time:" + (Date.now()-exp.trial_start))
            exp.counter = 0;
            $(".loc").unbind('click')
            _s.button();
          } else {
            exp.selection_array.push($(this).data().loc)
            exp.time_array.push(Date.now()-exp.trial_start)
            console.log("time:" + (Date.now()-exp.trial_start))
            $(".instruction").html(instruction_array[exp.counter])
            exp.counter++;
          }
        } else {
          //counter max below assumes no sentence will be longer than 5
          if (exp.counter>1){
            exp.selection_array.push($(this).data().loc)
            exp.time_array.push(Date.now()-exp.trial_start)
            console.log("time:" + (Date.now()-exp.trial_start))
            exp.counter = 0;
            $(".loc").unbind('click')
            _s.button();
          } else {
            exp.selection_array.push($(this).data().loc)
            exp.time_array.push(Date.now()-exp.trial_start)
            console.log("time:" + (Date.now()-exp.trial_start))
            $(".instruction").html(instruction_array[exp.counter])
            exp.counter++;
          }
        }
       });

    },

    button : function() {
      console.log("Location array => ",exp.selection_array)
      console.log("Time array => ",exp.time_array)
      this.log_responses();
      _stream.apply(this); /* use _stream.apply(this); if and only if there is
      "present" data. (and only *after* responses are logged) */
      
    },
    log_responses : function() {
    exp.data_trials.push({
        "trial" : this.stim.trial,
        "trialType" : this.stim.trialType,
        "trialID" : this.stim.trialID,
        "cond" : this.stim.cond,
        "target_pic": this.stim.target_pic,
        "target_contrast_good" : this.stim.target_contrast_good,
        "target_contrast_bad" : this.stim.target_contrast_bad,
        "big_filler" : this.stim.big_filler,
        "small_filler" : this.stim.small_filler,
        "loc_target_pic": loc_shuffled[0], 
        "loc_contrast": loc_shuffled[1],
        "loc_big_filler": loc_shuffled[2],
        "loc_small_filler": loc_shuffled[3],
        "instruction" : this.stim.instruction,
        "utterance_type" : this.stim.utterance_type,
        "response_times" : exp.time_array,
        "response" : exp.selection_array,
        "trial_number": exp.phase,
        "pragContext": pragContext
    });

    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };

  exp.stims_shuffled = _.shuffle(exp.stims_cb);
  console.log(exp.stims_cb);

  //blocks of the experiment:
  //exp.structure=["i0", "instructions", "practice", "afterpractice", "trial", 'subj_info', 'thanks'];
  exp.structure=["i0", "instructions", "trial", 'subj_info', 'thanks'];

  exp.data_trials = [];
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
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
