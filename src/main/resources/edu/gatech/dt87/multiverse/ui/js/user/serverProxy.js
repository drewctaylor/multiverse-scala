define(function() {
    return {
        initial : function() {
            return JSON.parse(server.initial());
        },

        satisfiableGoalSet: function (stateId) {
            return JSON.parse(server.satisfiableGoalSet(stateId));
        },

        satisfyGoal: function (stateId, goalId) {
            return JSON.parse(server.satisfyGoal(stateId, goalId));
        }
    };
});