state {

    character bebe {

    }

}

goal checkSceneForSuspense (character c) {
    // add suspense via character emotion
    strategy {

        "{{c.first}} was very scared."
    }

    // add suspense via failed escape
    strategy {


        "{{c.first} tried to escape, but failed."
    }
}

goal checkSceneForTragedy (character c) {
    // add add tragedy via loved one
    strategy {

    }
}

goal checkStoryForCharacterization {
    // add characterization statement
    strategy {

    }

    // add characterization example
    strategy {

    }
}

goal checkStoryForForeshadowing {
    // add foreshadowing
    strategy {

    }
}

goal checkConsistencyGoal {
    // make consistent supergoal
    strategy {

    }

    // make consistent motivating state
    strategy {

    }

    // make consistent p-health
    strategy {

    }

    // general motivating state
    strategy {

    }

    // check act preconditions
    strategy {
    }
}




goal eat(character c) {}
goal control(character c, object o) {}
goal go(character c, location l) {}
goal destroy(character c, object o) {}
goal scare(character c1, character c2) {}
goal status(character c) {}
goal love(character c) {}
goal affection(character c) {}
goal heal(character c) {}

goal schema (type, actor, object, priority)
    tell story
    check story for suspense
    check scene for suspense
    add suspense to scene
    check story for tragedy
    check scene for tragedy
    add tragedy to scene
    check story for foreshadowing
    check scene for foreshadowing
    add foreshadowing to scene
    check story for characterization
    add characterization
    check new scene
    check consistency
    check preconditions
    make goal consistent
    check affects
    add story intros
    add denouements
    connect
    instantiate
        instantiate-belief
        instantiate-evidence
        instantiate-evidence-2
        instantiate-superseding-belief
        instantiate-revenge
        instantiate-favor
        instantiate-anti-favor
        instantiate-thwarting-state
        instantiate-unthwarts
        instantiate-deception



goal type
    s-hunger
    d-control
    d-loc
    destroy
    scare
    a-status
    a-love
    a-affection
    c-health
    favor
    anti-favor
    retract
    deception

story schema (introduction scenes, body, denouement scenes)
theme schema (value, decision, consequence, connection, planner, active goals, current goal, current plan, world facts)

act schema (type, actor, object, to, from at, status)
    m-fight
    m-heal

state schema (type, object, value, to)
    health
    possess
    loc
    exist
    raise-goal
    affect
    kiss
    siblings
    know
    temper
    date
    role-change
    buried

belief schema (type, object, value, to)

human
    king
    knight
    princess
    hermit
    peasant

links
    subgoal
    subsumes
    plan
    intends
    unintended
    precond
    motivates
    achieves
    thwarts
    supersedes
    reaction
