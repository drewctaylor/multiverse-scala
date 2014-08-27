package edu.gatech.dt87.scalaverse.story


abstract sealed class Gender(val name: String);

case object MALE extends Gender("male");

case object FEMALE extends Gender("female");