package com.example.easyreader3.classes


import com.example.easyreader3.presentation.adapters.base.BaseItem

data class Book(
    var title:String="DefautTitle",
    var text:String="Defaut text.",
    var vocabulare:List<Word> = emptyList()
) : BaseItem {
    override fun title()= if(title.length>26)title.substring(0..26)else title

    override fun text()=if(text.length>500)text.substring(0..500)else text

    override fun text2()=if(title.length>40)title.substring(0..40)else title

    override fun info()=if(title.length>26)title.substring(0..26)else title

    override fun imgURL()=""


}