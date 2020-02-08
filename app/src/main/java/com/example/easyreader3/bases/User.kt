package com.example.easyreader3.bases

class User (
    var name: String="UserName",
    var email: String="",
    var age:Int=18,
    var vocabulare: Vocabulare= Vocabulare(),
    var exams:List<Exam> = listOf(),
    var scores: List<Map<String,Int>>

){}