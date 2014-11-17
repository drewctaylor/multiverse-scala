state {
    belief belief16 {
        type  := "deductive"
        actor := "lancelot"
    }

    fact state13 {
        type   := "affect"
        object := "andrea"
        to     := "richard"
        value  := "love"
    }

    belief16 -> state13 { mentalEvent := true }
}

goal tell() {
    strategy {
        belief b, fact f1 ?= (b -> f1).mentalEvent
        fact f2 !?= (f2 -> b).evidence

        instantiateEvidence(b, f1)
    }
}

goal instantiateEvidence(belief b, fact f1) {
    strategy {
        ?= b.actor != f1.object

        act a {
            actor := f1.object
            type := "mtrans"
            object := "belief"
            to := b.actor
        }

        fact f2 {
            type := "mbuild"
            object := b.actor
            value := "belief"
        }

        (a -> f2).intends := true
        (b -> f2).evidence := true
    }


    strategy {
        characterGoal g {
            actor := f1.object
        }

        (f1 -> g).motivates := true

        act a {
            actor := f1.object
        }

        (g -> a).plan := true

        fact f2 {
            type := "unknown"
        }

        (a  -> f2).intends := true
        (f2 -> g).achieves := true
        (f2 -> b).evidence := true

        "Test1"
    }
}
