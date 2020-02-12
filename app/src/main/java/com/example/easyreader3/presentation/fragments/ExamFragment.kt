package com.example.easyreader3.presentation.fragments


import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.classes.Exam
import com.example.easyreader3.presentation.adapters.base.BaseAdapterCallback
import com.example.easyreader3.presentation.fragments.BaseFragment
import com.example.easyreader3.classes.Question
import com.example.easyreader3.presentation.adapters.QuestionAdapter
import kotlinx.android.synthetic.main.include_recyclerview.*


class ExamFragment : BaseFragment() {
   val e= Exam()
    val adapter = QuestionAdapter()


    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        adapter.setList(hData.getBasicExam())
        adapter.attachCallback(object :
            BaseAdapterCallback<Question> {
            override fun onItemClick(model: Question, view: View) {
                Toast.makeText(activity,"itemClicked", Toast.LENGTH_SHORT).show()
            }

            override fun onLongClick(model: Question, view: View): Boolean {
                Toast.makeText(activity,"itemClonglicked",Toast.LENGTH_SHORT).show()
                return true
            }
        })
        return inflater.inflate(R.layout.fragment_exam, container, false)
    }

    override fun onStart() {
        rv.layoutManager = LinearLayoutManager(activity)
        rv.adapter = adapter
        super.onStart()
    }

    override fun onDestroyOptionsMenu() {
        adapter.detachCallback()
        super.onDestroyOptionsMenu()
    }

}