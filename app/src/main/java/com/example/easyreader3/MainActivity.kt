package com.example.easyreader3

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.fragment.app.FragmentManager
import androidx.fragment.app.FragmentTransaction
import androidx.navigation.fragment.NavHostFragment
import androidx.navigation.ui.setupWithNavController
import com.google.android.material.navigation.NavigationView
import kotlinx.android.synthetic.main.activity_main.*


class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
val host:NavHostFragment= supportFragmentManager
    .findFragmentById(R.id.navFragment) as NavHostFragment? ?: return
        val navController = host.navController


        val sideBar= findViewById<NavigationView>(R.id.nav_view)
        sideBar?.setupWithNavController(navController)
//        val fm: FragmentManager = supportFragmentManager
//        val fragment = RegistrationFragment()
//        val ft: FragmentTransaction = fm.beginTransaction()
//        ft.add(am_fragment_container, fragment)
//        ft.commit()

    }
}
