require.config({
    paths: {
        "jquery": "vendor/com.jquery/jquery-2.1.1.min",
        "handlebars": "vendor/com.handlebarsjs/handlebars-v2.0.0",
        "serverProxy": "user/serverProxy"
    }
});

require(["jquery", "handlebars", "serverProxy"], function ($, handlebars, serverProxy) {
    $(document).ready(function() {
        function satisfy(stateId, goalId, divScene) {
            var eventSequence = serverProxy.satisfyGoal(stateId, goalId).fabula

            var narrationArrayWithDuplicates = eventSequence.map(function(element) {
                return element.narration;
            });

            var narrationArray = narrationArrayWithDuplicates.reduce(function(previous, next) {
                if(previous.length === 0 || previous[previous.length - 1] !== next) {
                    return previous.concat(next);
                } else {
                    return previous;
                }
            }, []);

            var divSceneTextReplace = $("<div class=\"scene-text\">" + narrationArray.join("  ") + "</div>");
            $($(divScene).find(".scene-text")).replaceWith(divSceneTextReplace)

            var divSceneNext = $("<div class=\"scene\"></div>");
            var divPostitSet = $("<div class=\"postit-set\"></div>");
            var divSceneText =  $("<div class=\"scene-text\"></div>");

            divSceneNext.append(divPostitSet);
            divSceneNext.append(divSceneText);

            var goalSet = serverProxy.satisfiableGoalSet(eventSequence[eventSequence.length - 1].stateId);

            goalSet.forEach(function(goal) {
                var divPostit = $("<div class=\"postit left\">" + goal.goalName + "</div>");
                divPostit.click(function() {
                    divPostitSet.children().removeClass("selected");
                    divPostit.addClass("selected");
                    divSceneNext.nextAll().remove();
                    satisfy(eventSequence[eventSequence.length -1].stateId, goal.goalId, divSceneNext);
                });
                divPostitSet.append(divPostit);
            });

            $("#screenplay").append(divSceneNext);
        }

        var state = serverProxy.initial();
        var goalSet = serverProxy.satisfiableGoalSet(state.stateId);

        var divScene = $("<div class=\"scene\"></div>");
        var divPostitSet = $("<div class=\"postit-set\"></div>");
        var divSceneText = $("<div class=\"scene-text\"></div>");

        divScene.append(divPostitSet);
        divScene.append(divSceneText);

        goalSet.forEach(function(goal) {
            var divPostit = $("<div class=\"postit left\">" + goal.goalName + "</div>");
            divPostit.click(function() {
                divPostitSet.children().removeClass("selected");
                divPostit.addClass("selected");
                divScene.nextAll().remove();
                satisfy(0, goal.goalId, divScene);
            });
            divPostitSet.append(divPostit);
        });

        $("#screenplay").append(divScene)
    });
});