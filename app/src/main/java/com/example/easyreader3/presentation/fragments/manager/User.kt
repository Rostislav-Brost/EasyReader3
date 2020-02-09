package com.example.easyreader3.presentation.fragments.manager

import com.example.easyreader3.presentation.fragments.exam.Exam
import com.example.easyreader3.bases.Vocabulare
import com.example.easyreader3.bases.BaseItem

class User (
    var name: String="UserName",
    var email: String="",
    var age:Int=18,
    var vocabulare: Vocabulare = Vocabulare(),
    var exams:List<Exam> = listOf(),
    var scores: List<Map<String,Int>> = listOf(),
    var img:String =""

): BaseItem {
    override fun title(): String =name
    override fun text(): String =email
    override fun text2(): String ="("+age.toString()+"года)"
    override fun info()=""
    override fun imgURL(): String =img
}