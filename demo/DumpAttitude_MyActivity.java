package com.example.zsc.dumpattitude;

import android.app.Activity;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.net.http.AndroidHttpClient;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.provider.Settings.Secure;
import android.telephony.TelephonyManager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;

import java.io.FileOutputStream;
import java.io.IOException;


public class MyActivity extends Activity implements SensorEventListener {

    private static final String TAG = MyActivity.class.getSimpleName();
    private SensorManager m_sensor_manager;
    private Sensor m_rotation_vector;
    private int ipAddress;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_my);

        m_sensor_manager = (SensorManager) getSystemService(SENSOR_SERVICE);
        m_rotation_vector = m_sensor_manager.getDefaultSensor(Sensor.TYPE_ROTATION_VECTOR);

        WifiManager wifiManager = (WifiManager) getSystemService(WIFI_SERVICE);
        WifiInfo wifiInfo = wifiManager.getConnectionInfo();
        ipAddress = wifiInfo.getIpAddress();
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.my, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();
        if (id == R.id.action_settings) {
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    protected void onResume() {
        super.onResume();
        m_sensor_manager.registerListener(this, m_rotation_vector, SensorManager.SENSOR_DELAY_NORMAL);
    }

    protected void onPause() {
        super.onPause();
        m_sensor_manager.unregisterListener(this);
    }

    private class NetworkTask extends AsyncTask<String, Void, HttpResponse> {
        @Override
        protected HttpResponse doInBackground(String... params) {
            String link = params[0];
            HttpGet request = new HttpGet(link);
            AndroidHttpClient client = AndroidHttpClient.newInstance("Android");
            try {
                return client.execute(request);
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            } finally {
                client.close();
            }
        }

        @Override
        protected void onPostExecute(HttpResponse result) {
//            //Do something with result
//            if (result != null)
//                result.getEntity().writeTo(new FileOutputStream(f));
        }
    }

    public void onSensorChanged(SensorEvent event)
    {
//        Log.d(TAG, "onSensorChanged");
        String url = String.format(
                "http://192.168.2.166:1337/?id=%d&q0=%f&q1=%f&q2=%f", ipAddress,
                event.values[0], event.values[1], event.values[2]);
        new NetworkTask().execute(url);
        Log.d(TAG, "" + url);

    }

    public void onAccuracyChanged(Sensor sensor, int accuracy)
    {

    }
}
