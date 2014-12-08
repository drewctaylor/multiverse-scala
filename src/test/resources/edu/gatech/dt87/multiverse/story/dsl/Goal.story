~

given a character goal related to the character's health and part of the plot of the story, (alp-check-scene-for-suspense)
    add suspense

given a character goal related to the character's health and part of the plot of the story,
    1. (alp-add-suspense-via-emotion)
    add a story state in which the type is affect, the object is the character, the value is negative, the scale is normal, and the motivation is the motivation for the goal
    2. (alp-add-suspense-via-failed-escape)
    add an action in which the actor is the character, the plan is the p-health goal.
    add an author goal to instantiate the action
    satisfy the author goal
    make the final step a failure

given a character goal that is a health goal that is unintentionally thwarted by a character action and does the character have a character flaw or a bad past act (alp-check-for-tragedy)
    add tragedy

given the above, if the death was caused by another character
    1. (alp-add-tragedy-via-loved-one)
    add a story state in which the type is affect, the to is the flawed character, the object is the other character, the value is positive, the scale is strong.
    add an author goal to instantiate the action
    satsify the author goal in establishing scene
    add a story state in which the type is affect, the object is the other character, the value is negative, the scale is strong, and the reaction is to the death of the flawed character

given a character with a character trait (alp-check-story-for-characterization)
    1. (alp-add-characterization-via-statement)
    add characterization statement to introductory scenes
    2. (alp-add-characterization-via-example)
    add a character goal in which the actor is the character and the motivation is the character trait
    add an author goal to instantiate the goal
    satisfy the author goal
    add the instantiated goal to the story




given a character goal, (alp-check-consistency-goal)
    if the character goal is among the role goals for the character or
        the character goal is among the subgoals of another of the character's goals or
        the character goal is motivated by the state fo the world
    then
        do nothing;
    otherwise
        make the character goal consistent

given an inconsistent character goal, either
    1. (alp-make-consistent-goal)
    add a character goal such that the inconsistent character goal is a subgoal of the added character goal
    add an author goal to instantiate the character goal
    satisfy the author goal
    2. (alp-make-consistent-motivating-state)
    add a story state such that the inconsistent character goal is motivated by the added story state
    add an authro goal to instantiate the story state
    satisfy the author goal

given a state in which a character is injured or killed (alp-make-consistent-p-health)
    add a character goal such that the actor is the character, the type is protect-health, and the motivating state is the given state

given a state that does not motivate any character goal (alp-general-motivating-state)
    add a character goal such that motivating state is the given state
    add an author goal to instantiate the goal
    satisfy the author goal

given an action, (alp-check-act-preconds-goal)
    for each precondition that is not true, add a state, add the precondition to that state, and connect the state to the
    action by the precondition link.

alp-check-consistency-state
    is the object a state schema
    does it represent the existence of an object or location
    is the state a result of a character action
    if neither 2 nor 3 are true, then create an author level goal to make a consistent state
    otherwise it's consistent

alp-make-consistent-state
    make the object the result of a charactera ction
        make a new action
        connect the action to the character by the intends link
    use author level planning to instantiate the character action
        make a goal with instantiation of the schema
        call author level planning to acheive that goal
    if it succeeds add the action

alp-make-consistent-colocation
    (puts two people together)

alp-check-affects
    is the object a resolved goal schema that lacks an emotional reaction
    create a new state schema
        make a new state schema
        set the type to affect
        set the object to the actor of the goal
        connect the state to the goal by the g-situ link
    determine the strength of the reaction
        if the goal is important (p-health, c-health, or a-love), the strength is strong
        otherwise the strength is normal
        set the scale feature of the state to the strength
    determine the character the emotion is directed twoard
        if the goal was achieve or thwarded by intentional action, the emotion is directed toward the actor
        if unintentional, then the emotion is directed toward the actor, but the strength is weak
        set the to feature of the schema to the selected character

alp-check-affects-others (not implemented)
    is the object a resolved goal schema
    does the actor have a strong reaction
    if so, for each third-person character, look through the story to see if there exists any emotional relations between
    the character and the actor of the goal
    if positive, the create a normal emotional reaction equivalent to that of the actor
    if negative, then create a normal emotionl reation to this goal opposite to that of the actor



alp-check-affects-others

~
