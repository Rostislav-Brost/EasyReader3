package com.example.easyreader3.presentation.fragments

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.presentation.adapters.base.BaseAdapterCallback
import com.example.easyreader3.classes.Book
import com.example.easyreader3.data.local.HardData
import com.example.easyreader3.presentation.adapters.BookAdapter
import kotlinx.android.synthetic.main.include_recyclerview.*

class LibraryFragment : BaseFragment() {
    val userLibrary= ArrayList<Book>()
    val bookAdapter = BookAdapter()


    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
 userLibrary.addAll(hData.getFakeLibrary())
        bookAdapter.attachCallback(object :
            BaseAdapterCallback<Book> {
            override fun onItemClick(model: Book, view: View) {
                Toast.makeText(activity, "Clicked", Toast.LENGTH_SHORT).show()
            }
            override fun onLongClick(model: Book, view: View): Boolean {
                Toast.makeText(activity, "LongClicked", Toast.LENGTH_SHORT).show()
                return true
            }
        })

        return inflater.inflate(R.layout.fragment_library, container, false)
    }

    override fun onStart() {
        rv.layoutManager=LinearLayoutManager(activity)
        bookAdapter.setList(userLibrary)
        rv.adapter = bookAdapter
        super.onStart()
    }
    override fun onDestroyOptionsMenu() {
        bookAdapter.detachCallback()
        super.onDestroyOptionsMenu()
    }
}
