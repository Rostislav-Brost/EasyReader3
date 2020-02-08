package com.example.easyreader3.bases


/*класс экзамена , принимает на вход список вопросов

*
*

*
* содержит
* идентификатор
* название
* описание
* финальный текст
*
* список вопросов
* текуший номер вопроса
* счетчик правильных ответов
* счетчик неверных ответов
*
*Методы

*
* */
class Exam(
    val title: String = "ExamTitle",
    val description: String = "This is text. You can type some info about exam here.Try It!",
    //   val id:Int=1,
    val resultText: String = "Wow It is result text!",
    var questions: ArrayList<Question> = ArrayList<Question>(),
    var currentQuestion: Int = 0

) {

    //установить список
    //обновить вопрос
    fun updateQuestion(p: Int, q: Question) {
        if (p >= 0 && p < questions.size) questions[p] = q
    }

    //добавить вопрос
    fun addQuestion(q: Question) {
        questions.add(q)
    }

    //удалить вопрос
    fun removeQuestion(position: Int) {
        questions.removeAt(position)
    }

    //вернуть результат
    fun getResult(): String {
        var r = "Wow! its your result"

        return r
    }

    //вернуть количество верных ответов
    fun score(type: String = ""): String = when (type) {
        "%" -> {
            ""
        }//в процентах 70%
        "/" -> {
            ""
        } // в дроби 7/10
        "" -> {
            "defautresultScore"
        }// в абсолютном количестве 7
        else -> {
            ""
        }
    }


    //вернуть количество ошибок
    fun mistakeCount(): Int {
        var r: Int = 0

        return r
    }

    //вернуть успешность сдачи экзамена
    fun isPassed(): Boolean {
        var r: Boolean = false

        return r
    }

    //вернуть экзамен с неверными вопросами
    fun getFixWorkExam(): Exam {
        var r: Exam = Exam(
            title,
            description,
            resultText,
            questions/*ВОТ ТУТ ВСТАВИТЬ СПИСОК НЕПРАВИЛЬНЫХ ВОПРОСОВ*/,
            0
        )

        return r
    }
    //вернурнуть список правильных ответов
    fun getRightAnswers(): List<List<String>> {
        var r: ArrayList<List<String>> = ArrayList<List<String>>()

        return r
    }


}