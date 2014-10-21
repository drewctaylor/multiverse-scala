//package edu.gatech.dt87.scalaverse.story.character
//
///**
// * Extension methods for strings.
// */
//object StringExtension {
//
//    /**
//     * Extension methods that return a gender for a gender string.
//     *
//     * @param genderString a gender string
//     */
//    implicit class Gender(genderString: String) {
//
//        /**
//         * @return given a string, return the gender, if any.
//         */
//        def toGender: CharacterGender = genderString match {
//            case "MALE" => MALE
//            case "FEMALE" => FEMALE
//            case "NEUTER" => NEUTER
//            case _ => throw new IllegalArgumentException("The system cannot convert the given String to a Gender.")
//        }
//    }
//
//}