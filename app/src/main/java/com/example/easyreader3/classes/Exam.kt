package com.example.easyreader3.classes

import com.example.easyreader3.presentation.adapters.base.BaseItem

class Exam(
    val title: String = "Проверь себя!",
    val description: String = "Посмотрим, как ты освоил материал!"+"\n"+"Отметь в каждом слове ниже правильный перевод",
    val resultText: String = "Тестирование окончено! Результат сохранится в профиль",
    var questions: List<Question> = ArrayList<Question>()
) :BaseItem{
    override fun title()=title
    override fun text()=description
    override fun text2()=resultText
    override fun info()=""
    override fun imgURL()=""
}