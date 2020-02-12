package com.example.easyreader3.classes

import com.example.easyreader3.presentation.adapters.base.BaseItem

class User (
    var name: String="UserName",
    var email: String="",
    var age:Int=18,

    var vocabulare:List<Word> = emptyList(),
    val library:List<Book> = emptyList(),
    var scores:Int=0,
    var img:String =""

): BaseItem {
    override fun title(): String =name
    override fun text(): String =email
    override fun text2(): String ="("+age.toString()+"года)"
    override fun info()=""
    override fun imgURL(): String =img
}