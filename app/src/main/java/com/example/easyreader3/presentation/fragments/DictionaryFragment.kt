package com.example.easyreader3.presentation.fragments

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.presentation.adapters.base.BaseAdapterCallback
import com.example.easyreader3.classes.Word
import com.example.easyreader3.data.local.HardData
import com.example.easyreader3.presentation.adapters.WordAdapter
import kotlinx.android.synthetic.main.include_recyclerview.*

class DictionaryFragment : BaseFragment() {
    val recyclerList= ArrayList<Word>()
    val adapter = WordAdapter()


    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        recyclerList.addAll(hData.getGlobalDictionary(3))
        adapter.setList(recyclerList)
        adapter.attachCallback(object :
            BaseAdapterCallback<Word> {
            override fun onItemClick(model: Word, view: View) {
             Toast.makeText(activity,"itemClicked",Toast.LENGTH_SHORT).show()
            }
            override fun onLongClick(model: Word, view: View): Boolean {
                Toast.makeText(activity,"itemLongClicked",Toast.LENGTH_SHORT).show()
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
