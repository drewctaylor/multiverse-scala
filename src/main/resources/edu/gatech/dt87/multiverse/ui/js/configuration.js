require.config({
    paths: {
        "jquery": "vendor/com.jquery/jquery-2.1.1.min",
        "handlebars": "vendor/com.handlebarsjs/handlebars-v2.0.0",
        "serverProxy": "user/serverProxy"
    }
});

require(["jquery", "handlebars", "serverProxy"], function ($, handlebars, serverProxy) {
    $(document).ready(function () {

        serverProxy.initial(function(state) {

            function satisfy(stateId, goalId, divScene) {

                serverProxy.satisfyGoal(stateId, goalId, function(data) {

                    var eventSequence = data.fabula;

                    var narrationArrayWithDuplicates = eventSequence.map(function (element) {
                        return element.narration;
                    });

                    var narrationArray = narrationArrayWithDuplicates.reduce(function (previous, next) {
                        if (previous.length === 0 || previous[previous.length - 1] !== next) {
                            return previous.concat(next);
                        } else {
                            return previous;
                        }
                    }, []);

                    var divSceneTextReplace = $("<div class=\"scene-content\">" + narrationArray.join("  ") + "</div>");
                    $($(divScene).find(".scene-content")).replaceWith(divSceneTextReplace)

                    var divSceneNext = $("<div class=\"scene\"></div>");
                    var divPostitSet = $("<div class=\"postit-set\"></div>");
                    var divSceneText = $("<div class=\"scene-content\"></div>");
                    var divSeparator = $("<hr>");

                    divSceneNext.append(divPostitSet);
                    divSceneNext.append(divSceneText);

                    serverProxy.satisfiableGoalSet(eventSequence[eventSequence.length - 1].stateId, function(goalSet) {

                        goalSet.sort(function (a, b) {
                            return a.goalName.localeCompare(b.goalName)
                        }).forEach(function (goal) {
                            var divPostit = $("<div class=\"postit left\"><div>" + goal.goalName + "</div></div>");
                            divPostit.click(function () {
                                divPostitSet.children().removeClass("selected");
                                divPostit.addClass("selected");
                                divSceneNext.nextAll().remove();
                                divSceneNext.append(divSeparator);
                                satisfy(eventSequence[eventSequence.length - 1].stateId, goal.goalId, divSceneNext);
                            });
                            divPostitSet.append(divPostit);
                        });

                        $("#page").append(divSceneNext);
                    });
                });
            }

            $(".title").text(state.title);
            $("title").text(state.title);

            serverProxy.satisfiableGoalSet(state.stateId, function(goalSet) {

                var divScene = $("<div class=\"scene\"></div>");
                var divPostitSet = $("<div class=\"postit-set\"></div>");
                var divSceneText = $("<div class=\"scene-content\"></div>");
                var divSeparator = $("<hr>");

                divScene.append(divPostitSet);
                divScene.append(divSceneText);

                goalSet.sort(function (a, b) {
                    return a.goalName.localeCompare(b.goalName)
                }).forEach(function (goal) {
                    var divPostit = $("<div class=\"postit left\"><div>" + goal.goalName + "</div></div>");
                    divPostit.click(function () {
                        divPostitSet.children().removeClass("selected");
                        divPostit.addClass("selected");
                        divScene.nextAll().remove();
                        divScene.append(divSeparator);
                        satisfy(0, goal.goalId, divScene);
                    });
                    divPostitSet.append(divPostit);
                });

                $("#page").append(divScene)
            })
        });
    });
});