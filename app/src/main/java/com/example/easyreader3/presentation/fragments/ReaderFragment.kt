package com.example.easyreader3.presentation.fragments


import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.classes.Book
import com.example.easyreader3.data.local.HardData
import com.example.easyreader3.presentation.adapters.StringAdapter
import com.example.easyreader3.presentation.adapters.base.BaseAdapterCallback
import kotlinx.android.synthetic.main.include_recyclerview.*

class ReaderFragment : BaseFragment() {

   // val userLibrary= ArrayList<Book>()
   var currentBook=Book()
    val bookAdapter = StringAdapter()


    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        currentBook=hData.getFakeLibrary()[0]
        bookAdapter.attachCallback(object :
            BaseAdapterCallback<String> {
            override fun onItemClick(model: String, view: View) {
                Toast.makeText(activity, "Clicked", Toast.LENGTH_SHORT).show()
            }
            override fun onLongClick(model: String, view: View): Boolean {
                Toast.makeText(activity, "LongClicked", Toast.LENGTH_SHORT).show()
                return true
            }
        })

        return inflater.inflate(R.layout.fragment_reader, container, false)
    }

    override fun onStart() {
        rv.layoutManager= LinearLayoutManager(activity)
        bookAdapter.setList(currentBook)
        rv.adapter = bookAdapter
        super.onStart()
    }
    override fun onDestroyOptionsMenu() {
        bookAdapter.detachCallback()
        super.onDestroyOptionsMenu()
    }


}
