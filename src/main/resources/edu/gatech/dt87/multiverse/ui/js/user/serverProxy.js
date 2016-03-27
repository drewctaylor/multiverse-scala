define(function() {
    return {
        initial : function(f) {
            $.get("initial", function(data) {
                f(JSON.parse(data))
            });
        },

        satisfiableGoalSet: function (stateId, f) {
            $.get("satisfiableGoalSet/" + stateId, function(data) {
                f(JSON.parse(data))
            });
        },

        satisfyGoal: function (stateId, goalId, f) {
            $.get("satisfyGoal/" + stateId + "/" + goalId, function(data) {
                f(JSON.parse(data))
            });
        }
    };
});