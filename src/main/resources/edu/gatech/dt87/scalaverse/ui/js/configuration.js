require.config({
    paths: {
        "jquery": "vendor/com.jquery/jquery-2.1.1.min",
        "handlebars": "vendor/com.handlebarsjs/handlebars-v2.0.0",
        "serverProxy": "user/serverProxy"
    }
});

require(["jquery", "handlebars", "serverProxy"], function ($, handlebars, serverProxy) {
    $(document).ready(function() {
        function satisfy(stateId, goalId) {
            var divScene = $("<div class=\"scene\"></div>");

            var eventSequence = serverProxy.satisfyGoal(stateId, goalId);

            var divPostitSet = $("<div class=\"postit-set\"></div>");

            divScene.append(divPostitSet);

            var goalSet = serverProxy.satisfiableGoalSet(eventSequence[eventSequence.length - 1].stateId);
            goalSet.forEach(function(goal) {
                var divPostit = $("<div class=\"postit left\">" + goal.goalName + "</div>");
                divPostit.click(function() {
                    divPostitSet.children().removeClass("selected");
                    divPostit.addClass("selected");
                    divScene.nextAll().remove();
                    satisfy(eventSequence[eventSequence.length -1].stateId, goal.goalId);
                });
                divPostitSet.append(divPostit);
            });

            eventSequence.forEach(function(element) {
                var div = $("<div class=\"scene-text\"><div class=\"dialog\">" + element.narration + "</div></div>");
                divScene.append(div);
            });


            $("#screenplay").append(divScene);
        }

        function append(state) {
            var goalSet = serverProxy.satisfiableGoalSet(state.stateId);

            var divScene = $("<div class=\"scene\"></div>");
            var divPostitSet = $("<div class=\"postit-set\"></div>");

            divScene.append(divPostitSet);

            goalSet.forEach(function(goal) {
                var divPostit = $("<div class=\"postit left\">" + goal.goalName + "</div>");
                divPostit.click(function() {
                    divPostitSet.children().removeClass("selected");
                    divPostit.addClass("selected");
                    divScene.nextAll().remove();
                    satisfy(state.stateId, goal.goalId);
                });
                divPostitSet.append(divPostit);
            });

            var divSceneText = $("<div class=\"scene-text\"><div class=\"dialog\">" + state.narration + "</div></div>")
            divScene.append(divSceneText);

            $("#screenplay").append(divScene);
        }

        append(serverProxy.initial());
    });
});