package com.example.easyreader3.presentation.fragments.exam

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import com.example.easyreader3.R
import com.example.easyreader3.bases.BaseAdapter
import com.example.easyreader3.bases.BaseViewHolder
import kotlinx.android.synthetic.main.item_question.view.*

class QuestionAdapter : BaseAdapter<Question>() {
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): BaseViewHolder<Question> {
        return ViewHolder(LayoutInflater.from(parent.context).inflate(R.layout.item_question,parent,false))
    }

    class ViewHolder(itemView: View): BaseViewHolder<Question>(itemView){
        override fun bind(model: Question) {
            itemView.iq_question.text=model.title
            itemView.iq_answer1.text=model.answers[0]
            itemView.iq_answer2.text=model.answers[1]
            itemView.iq_answer3.text=model.answers[2]
            itemView.iq_answer1.setOnClickListener { answerClicked(0) }
            itemView.iq_answer1.setOnClickListener { answerClicked(1) }
            itemView.iq_answer1.setOnClickListener { answerClicked(2) }

        }
        fun answerClicked(number:Int){

        }

    }

}