package com.example.easyreader3.presentation.fragments.exam


import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.bases.BaseAdapterCallback
import com.example.easyreader3.bases.BaseFragment
import com.example.easyreader3.presentation.fragments.dictionary.Word
import kotlinx.android.synthetic.main.include_recyclerview.*


class ExamFragment : BaseFragment() {
    val recyclerList = ArrayList<Question>()
    val adapter = QuestionAdapter()

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        recyclerList.addAll(
            listOf(
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3")),
                Question("Word", answers = listOf("ans1", "ans2", "ans3"))

            )
        )
        adapter.setList(recyclerList)
        adapter.attachCallback(object :
            BaseAdapterCallback<Question> {
            override fun onItemClick(model: Question, view: View) {
                toast("itemClicked")
            }

            override fun onLongClick(model: Question, view: View): Boolean {
                toast("itemLongClicked")
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
