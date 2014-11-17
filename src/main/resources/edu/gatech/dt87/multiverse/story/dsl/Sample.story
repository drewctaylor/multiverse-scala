state {
    character matt {
        first := "Matt"
        life := "alive"
        gender := "male"
        orientation := "male"
    }

    character jake {
    first := "Jake"
        life := "alive"
        gender := "male"
        orientation := "male" + "female"
    }

    character alison {
    first := "Alison"
        life := "alive"
        gender := "female"
        orientation := "male"
    }

    character peter {
    first := "Peter"
        life := "alive"
        gender := "male"
        orientation := "female"
    }

    character michael {
    first := "Michael"
        life := "alive"
        gender := "male"
        orientation := "female"
    }

    character kimberly {
    first := "Kimberly"
        life := "alive"
        gender := "female"
        orientation := "male"
    }

    michael <-> kimberly {
        marriage := "marriage"
    }
}

goal marriage {
    strategy {
        character c1, character c2 ?= (c1 <-> c2).marriage != "marriage" && c1 != c2 && (c1.gender [ c2.orientation) && (c2.gender [ c1.orientation)
        satisfy alive c1
        satisfy alive c2
        satisfy single c1
        satisfy single c2
        satisfy marry c1, c2
        (c1<->c2).test := "test"
    }
}

goal divorce {
    strategy {
        character c1, character c2 ?= (c1 <-> c2).marriage == "marriage"
        (c1 <-> c2).marriage := "divorce"
        "{{c2.first}} and {{c1.first}} divorce."
    }
}

goal alive character c {
    strategy {
        ?= c.life == "alive"
        "({{c.first}} is alive.)"
    }
    strategy {
        ?= c.life == "dead"
        c.life := "alive"
        "{{c.first}} returns to Melrose Place, very much alive."
    }
}

goal single character c1 {
    strategy {
        character c2 ?= (c1 <-> c2).marriage == "marriage"
        (c1 <-> c2).marriage := "divorce"
        c2.life := "dead"
        "{{c2.first}} dies, leaving {{c1.first}} alone."
    }

    strategy {
        character c2 ?= (c1 <-> c2).marriage == "marriage"
        (c1 <-> c2).marriage := "divorce"
        "{{c2.first}} and {{c1.first}} divorce."
    }

    strategy {
        character c2 !?= (c1 <-> c2).marriage == "marriage"
        "({{c1.first}} is single.)"
    }
}

goal marry character c1, character c2 {
    strategy {

        (c1 <-> c2).marriage := "marriage"
        "{{c2.first}} and {{c1.first}} marry."
    }
}