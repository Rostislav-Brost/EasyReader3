package com.example.easyreader3.bases

import android.widget.Toast
import androidx.fragment.app.Fragment


open class BaseFragment(): Fragment() {
fun toast(text:String){Toast.makeText(activity,text,Toast.LENGTH_SHORT).show()}






}
