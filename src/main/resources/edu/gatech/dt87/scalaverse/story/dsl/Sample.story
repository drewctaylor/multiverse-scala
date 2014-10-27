state {
    attribute first one symbol
    attribute last one symbol

    attribute gender one of unordered set { "female" "male" }

    attribute age                 one of ordered set { "baby" "child" "teen" "young adult" "adult" "elder adult" }
    attribute wealth              one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute promiscuity         one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute competence          one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute niceness            one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute self-confidence     one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute guile               one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute moodiness           one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute physical-appearance one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute intelligence        one of ordered set { "very-low" "low" "medium" "high" "very-high" }

    attribute rel-positive        one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute rel-intimate        one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute rel-dominance       one of ordered set { "very-low" "low" "medium" "high" "very-high" }
    attribute rel-attract         one of ordered set { "very-low" "low" "medium" "high" "very-high" }

    attribute character-goal      subset of unordered set { "become-famous" "meet-famous-people" "associate-right" "find-happiness" }

    attribute relationship-type   one of unordered set { "marriage" "divorce" }
    attribute family-relationship one of unordered set { "parent->child" "child->parent" }


    character jessica {
        first               := "Jessica"
        last                := "Donadio"
        gender              := "female"
        age                 := "adult"
        wealth              := "medium"
        promiscuity         := "high"
        competence          := "high"
        niceness            := "very low"
        self-confidence     := "high"
        guile               := "high"
        moodiness           := "medium"
        physical-appearance := "medium"
        intelligence        := "medium"
        character-goal      += "become-famous"
        character-goal      += "meet-famous-people"
        character-goal      += "associate-right"
        character-goal      += "find-happiness"
    }

    character douglas {
        first  := "Douglas"
        last   := "Davidson"
        gender := "male"
        age    := "adult"
    }

    character ivan {
        first  := "Ivan"
        last   := "Schaad"
        gender := "male"
        age    := "adult"
    }

    character mark {
        first  := "Mark"
        last   := "Davidson"
        gender := "male"
        age    := "young-adult"
    }

    character rene {
        first  := "Rene"
        last   := "Davidson"
        gender := "female"
        age    := "young-adult"
    }

    relationship jessica <-> douglas {
        relationship-type   := "divorce"
    }

    relationship jessica -> douglas {
        rel-positive  := "very-low"
        rel-intimate  := "medium"
        rel-dominance := "very-low"
        rel-attract   := "medium"
    }

    relationship douglas -> jessica {
        rel-positive  := "very-low"
        rel-intimate := "medium"
        rel-dominance := "very-low"
        rel-attract := "medium"
    }

    relationship jessica <-> ivan {
        relationship-type   := "divorce"
    }

    relationship jessica -> ivan {
        rel-positive  := "very-low"
        rel-intimate  := "medium"
        rel-dominance := "very-low"
        rel-attract   := "medium"
    }

    relationship ivan -> jessica {
        rel-positive  := "very-low"
        rel-intimate  := "medium"
        rel-dominance := "very-low"
        rel-attract   := "medium"
    }

    relationship jessica -> mark {
        rel-positive  := "very-high"
        rel-intimate  := "medium"
        rel-dominance := "very-high"
        family-relationship := "parent-child"
    }

    relationship mark -> jessica {
        rel-positive  := "medium"
        rel-intimate  := "medium"
        rel-dominance := "very-low"
        family-relationship := "child-parent"
    }

    relationship jessica -> rene {
        rel-positive  := "very-high"
        rel-intimate  := "medium"
        rel-dominance := "very-high"
        family-relationship := "parent-child"
    }

    relationship rene -> jessica {
        rel-positive  := "medium"
        rel-intimate  := "medium"
        rel-dominance := "very-low"
        family-relationship := "child-parent"
    }
}

