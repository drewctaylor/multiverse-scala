
state {

    character bebe {
        name := "Bebe"
        denouement := false
        life := "alive"
        role := "hermit"
    }

    characterGoal bebeGoal {
        status := "failure"
    }

    bebe <-> bebeGoal {
        exists := true
    }

    character lancelot {
        name := "Lancelot"
        denouement := false
        life := "alive"
        role := "knight"
    }

    characterGoal lancelotGoal {
        status := "success"
    }

    lancelot <-> lancelotGoal {
        exists := true
    }
}

goal tell() {
    strategy {
        denouement()

        "The End"
    }
}

goal denouement() {
    strategy {
        character c !?= c.denouement == false
    }

    strategy {
        character c  ?= c.denouement == false

        denouementCharacter(c)

        c.denouement := true

        denouement()
    }
}

goal denouementCharacter(character c) {
    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "knight" &&
            g.status == "failure"

        c.role := "hermit"

        "{{c.name}} abandoned his sword and became a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "hermit" &&
            g.status == "failure"

        c.role := "knight"

        "{{c.name}} abandoned his vows and became a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "nun" &&
            g.status == "failure"

        c.role := "princess"

        "{{c.name}} abandoned her vows and became a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "princess" &&
            g.status == "failure"

        c.role := "nun"

        "{{c.name}} abandoned her crown and became a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "knight" &&
            g.status == "success"

        c.role := "king"

        "{{c.name}} ultimately became {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "hermit" &&
            g.status == "success"

        "{{c.name}} renewed his vows and remained a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "nun" &&
            g.status == "success"

        "{{c.name}} renewed her vows and remained a {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "alive" &&
            c.role == "princess" &&
            g.status == "success"

        c.role := "queen"

        "{{c.name}} ultimately became {{c.role}}."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "dead" &&
            g.status == "success"

        "{{c.name}} was buried with honor in the woods."
    }

    strategy {
        characterGoal g ?=
            (c<->g).exists == true &&
            c.life == "dead" &&
            g.status == "failure"

        "{{c.name}} was buried without honor in the woods."
    }
}