package com.example.easyreader3.presentation.adapters

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.example.easyreader3.R
import com.example.easyreader3.classes.Exam
import com.example.easyreader3.classes.Question
import com.example.easyreader3.presentation.adapters.base.BaseAdapter
import com.example.easyreader3.presentation.adapters.base.BaseViewHolder
import kotlinx.android.synthetic.main.item_exam_question.view.*
import kotlinx.android.synthetic.main.item_exam_result.view.*
import kotlinx.android.synthetic.main.item_exam_title.view.*

class QuestionAdapter : BaseAdapter<Question>() {
    val titleTag="MAGIC_TITLE_STRING_0"
    val footerTag="MAGIC_FOOTER_STRING_2"

    override fun getItemViewType(position: Int) =when(items[position].title){
        titleTag->0
        footerTag->2
        else->1
    }

    override fun onCreateViewHolder(vg: ViewGroup, viewType: Int): BaseViewHolder<Question> =when(viewType){
        0->{ViewHolder(LayoutInflater.from(vg.context).inflate(R.layout.item_exam_title, vg, false))}
        2->{ViewHolder(LayoutInflater.from(vg.context).inflate(R.layout.item_exam_result, vg, false))}
        else->{ViewHolder(LayoutInflater.from(vg.context).inflate(R.layout.item_exam_question, vg, false))}
    }







fun setList(e: Exam){
    items.clear()
    items.add(Question(titleTag,e.title,e.description))
    items.addAll(e.questions)
    items.add(Question(footerTag,e.resultText))
    notifyDataSetChanged()
}




    class ViewHolder(itemView: View): BaseViewHolder<Question>(itemView){
        val titleTag="MAGIC_TITLE_STRING_0"
        val footerTag="MAGIC_FOOTER_STRING_2"
        override fun bind(model: Question) {
            when (model.title){
                titleTag->{ itemView.iet_description.text=model.imgUrl
                    itemView.iet_title.text=model.text }
                footerTag->{itemView.ier_text.text =model.text }
                else->{
                    itemView.iq_question.text=model.text
                    itemView.iq_counter_tv.text= model.title
                    itemView.iq_answer1.text=model.answers[0]
                    itemView.iq_answer2.text=model.answers[1]
                    itemView.iq_answer3.text=model.answers[2]
                    itemView.iq_answer1.setOnClickListener { answerClicked(0) }
                    itemView.iq_answer1.setOnClickListener { answerClicked(1) }
                    itemView.iq_answer1.setOnClickListener { answerClicked(2) }
                }
            }
        }
        fun answerClicked(number:Int){
val isRight:Boolean= true




        }


    }

}