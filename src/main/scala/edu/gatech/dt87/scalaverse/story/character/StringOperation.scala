package edu.gatech.dt87.scalaverse.story.character

class StringOperation(string : String) {
    def toGender : CharacterGender = {
        if(string == "MALE") {
            MALE
        } else if(string == "FEMALE") {
            FEMALE
        } else {
            throw new IllegalArgumentException("The system cannot convert the given String to a Gender.")
        }
    }
}

object StringOperation {
    implicit def wrap(string : String) : StringOperation = {
        new StringOperation(string)
    }
}