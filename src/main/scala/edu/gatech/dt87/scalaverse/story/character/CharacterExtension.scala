//package edu.gatech.dt87.scalaverse.story.character
//
///**
// * Extension methods for characters.
// */
//object CharacterExtension {
//
//    /**
//     * Extension methods that return the capitalized and uncapitalized subject, object, and possessive pronouns and determiners for a character.
//     *
//     * @param character a character
//     */
//    implicit class Pronoun(character: Character) {
//
//        /**
//         * @return given a character, return the uncapitalized subject pronoun for that character's gender
//         */
//        def sub: String = character.attributeValueMap("gender") match {
//            case "male" => "he"
//            case "female" => "she"
//            case "neuter" => "it"
//        }
//
//        /**
//         * @return given a character, return the capitalized subject pronoun for that character's gender
//         */
//        def Sub: String = sub.capitalize
//
//        /**
//         * @return given a character, return the uncapitalized object pronoun for that character's gender
//         */
//        def obj: String = character.attributeValueMap("gender") match {
//            case MALE => "him"
//            case FEMALE => "her"
//            case NEUTER => "it"
//        }
//
//        /**
//         * @return given a character, return the capitalized object pronoun for that character's gender
//         */
//        def Obj: String = obj.capitalize
//
//        /**
//         * @return given a character, return the uncapitalized possessive pronoun for that character's gender
//         */
//        def pos: String = character.attributeValueMap("gender") match {
//            case MALE => "his"
//            case FEMALE => "hers"
//            case NEUTER => "its"
//        }
//
//        /**
//         * @return given a character, return the capitalized possessive pronoun for that character's gender
//         */
//        def Pos: String = pos.capitalize
//
//        /**
//         * @return given a character, return the uncapitalized determiner for that character's gender
//         */
//        def det: String = character.attributeValueMap("gender") match {
//            case MALE => "his"
//            case FEMALE => "her"
//            case NEUTER => "its"
//        }
//
//        /**
//         * @return given a character, return the capitalized determiner for that character's gender
//         */
//        def Det: String = det.capitalize
//    }
//
//}
