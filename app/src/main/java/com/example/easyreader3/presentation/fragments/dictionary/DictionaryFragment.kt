package com.example.easyreader3.presentation.fragments.dictionary

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.bases.BaseAdapterCallback
import com.example.easyreader3.bases.BaseFragment
import kotlinx.android.synthetic.main.include_recyclerview.*

class DictionaryFragment : BaseFragment() {
    val recyclerList= ArrayList<Word>()
    val adapter = WordAdapter()

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        recyclerList.addAll(listOf(
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat"),
           Word("cat",3423,"кошка","caet","This is my cat")
        ))
        adapter.setList(recyclerList)
        adapter.attachCallback(object : BaseAdapterCallback<Word> {
            override fun onItemClick(model: Word, view: View) {
                toast("itemClicked")
            }
            override fun onLongClick(model: Word, view: View): Boolean {
                toast("itemLongClicked")
                return true
            }
        })
        return inflater.inflate(R.layout.fragment_dictionary, container, false)
    }

    override fun onStart() {
       rv.layoutManager=  LinearLayoutManager(activity)
        rv.adapter =adapter
        super.onStart()
    }
    override fun onDestroyOptionsMenu() {
      adapter.detachCallback()
        super.onDestroyOptionsMenu()
    }

}
