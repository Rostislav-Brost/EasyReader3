package com.example.easyreader3.classes

import com.example.easyreader3.presentation.adapters.base.BaseItem

/*Класс слова хранит  количество использований, перевод, транскрибцию , контекст ,(возможно место и айдишник текста)*/
data class Word (val w:String="#",
            var counter:Int=1,
            val translate:String="",
            val transcribtion:String="",
    //val text id list
            val example:String=""

): BaseItem {
    override fun title()=w
    override fun text()="["+transcribtion+"]"
    override fun text2()=translate
    override fun info()="встречено $counter р."
    override fun imgURL()=example


}