/**
 * @brief FAST detector + ORB descriptor + BF Matcher + FindHomography
 *
 * @author A. Huaman
 * @author shuchang.zhou@gmail.com
 *
 * To be used with Mathematica m file below:
rowColumnToXy=Function[{row,column,height},{column-1/2,height-row-1/2}];
Clear[imageCorrespondingPoints];
imageCorrespondingPoints[image1_,image2_,opts_:{"Transformation"->"none"}]:=Module[{fnames,imgs={image1,image2},imgHeights},
	imgHeights=ImageDimensions[#][[2]]&/@imgs;
	fnames=Table[CreateTemporary[],{Length@imgs+1}];MapThread[Export[#,#2,"JPG"]&,{fnames[[;;2]],imgs}];
	Import["!/h/bin/opencv_matcher "<>fnames[[1]]<>" "<>fnames[[2]]<>" --output_fname="<>fnames[[3]]<>" --ransac_type="<>("Transformation"/.opts),"Lines"];
	Transpose[{rowColumnToXy[#[[1,2]]+1,#[[1,1]]+1,imgHeights[[1]]],
		rowColumnToXy[#[[2,2]]+1,#[[2,1]]+1,imgHeights[[2]]]}&@Partition[#,2]&/@Import@fnames[[3]]]
	];
 */

#include <stdio.h>
#include <fstream>
#include <iostream>
#include <Eigen/Core>
#include "gflags/gflags.h"
#include "glog/logging.h"
#include "opencv2/calib3d/calib3d.hpp"
#include "opencv2/core/core.hpp"
#include "opencv2/features2d/features2d.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

using Eigen::Map;
using Eigen::Matrix3d;
using Eigen::Matrix3f;
using Eigen::Vector3d;
using Eigen::Vector3f;

DEFINE_string(task, "draw_matches", "");
//DEFINE_string(attitudes_file, "", "");
//DEFINE_string(method, "plane_set", "pure_vision/plane_set/incremental");
DEFINE_string(match_method, "KLT", "KLT/ORB");
DEFINE_bool(draw_matches, false, "");
DEFINE_string(output_fname, "", "");
DEFINE_string(ransac_type, "none", "none/Perspective/Epipolar");

using namespace std;
using namespace cv;

void RansacTest(
    const vector<cv::DMatch>& matches,
    const vector<cv::KeyPoint>& keypoints1,
    const vector<cv::KeyPoint>& keypoints2,
    double distance,
    double confidence,
    bool is_homography,
    double matrix[9],
    vector<cv::DMatch>* goodMatches) {
  goodMatches->clear();
  // Convert keypoints into Point2f
  vector<cv::Point2f> points1, points2;
  for (vector<cv::DMatch>::const_iterator it= matches.begin();
      it!= matches.end(); ++it)
  {
      // Get the position of left keypoints
      float x= keypoints1[it->queryIdx].pt.x;
      float y= keypoints1[it->queryIdx].pt.y;
      points1.push_back(cv::Point2f(x,y));
      // Get the position of right keypoints
      x= keypoints2[it->trainIdx].pt.x;
      y= keypoints2[it->trainIdx].pt.y;
      points2.push_back(cv::Point2f(x,y));
  }
  // Compute F matrix using RANSAC
  vector<uchar> inliers(points1.size(),0);
  cv::Mat learnt_matrix;
  if (is_homography) {
    learnt_matrix = cv::findHomography(
        cv::Mat(points1),cv::Mat(points2),inliers, cv::RANSAC, 5.0);
  } else {
    learnt_matrix = cv::findFundamentalMat(
        cv::Mat(points1),cv::Mat(points2),inliers,CV_FM_RANSAC,
        distance,confidence);
  }

  if (NULL != matrix) {
    Map<Matrix3d> matrix_map(matrix);
    for (int i = 0; i < 3; ++i) {
      for (int j = 0; j < 3; ++j) {
        matrix_map(i, j) = learnt_matrix.at<double>(i, j);
      }
    }
  }
  // extract the surviving (inliers) matches
  vector<uchar>::const_iterator itIn= inliers.begin();
  vector<cv::DMatch>::const_iterator itM= matches.begin();
  // for all matches
  for ( ;itIn!= inliers.end(); ++itIn, ++itM)
  {
      if (*itIn)
      { // it is a valid match
          goodMatches->push_back(*itM);
      }
  }
}

void DumpData(const double* data, int num_observations) {
  std::ostringstream str;
  if (!FLAGS_output_fname.empty()) {
	std::ofstream out(FLAGS_output_fname.c_str(), std::ios::out);

    for (int i = 0; i < num_observations; ++i) {
      str << data[i];
      str << "\n";
    }
    out << str.str();
    out.close();
    LOG(INFO) << "Written data to file.";
  }
}

void DumpMatches(
		const vector<cv::KeyPoint>& keypoints_object,
		const vector<cv::KeyPoint>& keypoints_scene,
		const vector<DMatch>& matches, const char* ofname) {
	std::ofstream out(ofname, std::ios::out);
	for (int i = 0; i < matches.size(); ++i) {
		const DMatch& match = matches[i];
		out << keypoints_object[ match.queryIdx ].pt.x << ",";
		out << keypoints_object[ match.queryIdx ].pt.y << ",";
		out << keypoints_scene[ match.trainIdx ].pt.x << ",";
		out << keypoints_scene[ match.trainIdx ].pt.y;
		out << "\n";
	}
//	out << str.str();
	out.close();
	LOG(INFO) << "Written data to file.";
}

void WriteMatches(
    const Mat& img_object, const Mat& img_scene,
    const vector<cv::KeyPoint>& keypoints_object,
    const vector<cv::KeyPoint>& keypoints_scene,
    const vector< DMatch >& good_matches, const std::string& ofname) {
//  vector< DMatch > good_matches;
//  double t = (double)getTickCount();
//  if (matches.size() >= 4) {
//    RansacTest(
//        matches, keypoints_object, keypoints_scene, 2.0, 0.99, true,
//        NULL, &good_matches);
//  } else {
//    cerr << "matches.size() = " << matches.size();
//    good_matches = matches;
//  }
//  t = ((double)getTickCount() - t)/getTickFrequency();
//  LOG(INFO) << "RANSAC seconds: " << t;

  Mat img_matches;
  drawMatches( img_object, keypoints_object, img_scene, keypoints_scene,
               good_matches, img_matches, Scalar::all(-1), Scalar::all(-1),
               vector<char>(), DrawMatchesFlags::NOT_DRAW_SINGLE_POINTS );

  if (good_matches.size() >= 4) {
    //-- Localize the object from img_1 in img_2
    vector<Point2f> obj;
    vector<Point2f> scene;

    for( size_t i = 0; i < good_matches.size(); i++ )
    {
      //-- Get the keypoints from the good matches
      obj.push_back( keypoints_object[ good_matches[i].queryIdx ].pt );
      scene.push_back( keypoints_scene[ good_matches[i].trainIdx ].pt );
    }

    Mat H = findHomography( obj, scene, RANSAC );

    //-- Get the corners from the image_1 ( the object to be "detected" )
    vector<Point2f> obj_corners(4);
    obj_corners[0] = Point(0,0); obj_corners[1] = Point( img_object.cols, 0 );
    obj_corners[2] = Point( img_object.cols, img_object.rows ); obj_corners[3] = Point( 0, img_object.rows );
    vector<Point2f> scene_corners(4);

    perspectiveTransform( obj_corners, scene_corners, H);
    //-- Draw lines between the corners (the mapped object in the scene - image_2 )

    Point2f offset( (float)img_object.cols, 0);
    line( img_matches, scene_corners[0] + offset, scene_corners[1] + offset, Scalar(0, 255, 0), 4 );
    line( img_matches, scene_corners[1] + offset, scene_corners[2] + offset, Scalar( 0, 255, 0), 4 );
    line( img_matches, scene_corners[2] + offset, scene_corners[3] + offset, Scalar( 0, 255, 0), 4 );
    line( img_matches, scene_corners[3] + offset, scene_corners[0] + offset, Scalar( 0, 255, 0), 4 );
  }

  //-- Show detected matches
  imwrite( ofname , img_matches );
}

void GetFileContent(const char* filename, std::string* contents) {
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
    in.seekg(0, std::ios::end);
    contents->resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&(*contents)[0], contents->size());
    in.close();
    return;
  }
  std::string str = filename;
  cerr << "GetFileContent " << str << " failed.";
  exit(-1);
}

void SplitString(
    const std::string &s, char delim, std::vector<std::string>* elems) {
  elems->clear();
  std::stringstream ss(s);
  std::string item;
  while (std::getline(ss, item, delim)) {
      elems->push_back(item);
  }
}

// Only read some fields!
void LoadCsv(const char* fname, vector<vector<double> >* matrix) {
  std::string str;
  GetFileContent(fname, &str);
  vector<std::string> lines;
  SplitString(str, '\n', &lines);
  matrix->clear();
  vector<std::string> num_strs;
  vector<double> nums;
  matrix->reserve(lines.size());
  for (int i = 0; i < lines.size(); ++i) {
    SplitString(lines[i], ',', &num_strs);
    nums.clear();
    double num;
    nums.reserve(num_strs.size());
    for (int i = 1; i < 5; ++i) {
      stringstream is(num_strs[i]);
      is >> num;
      nums.push_back(num);
    }
    matrix->push_back(nums);
  }
}

void MatchImagePair(
    const Mat& img_object, const Mat& img_scene,
    vector<cv::KeyPoint>* keypoints_object,
    vector<cv::KeyPoint>* keypoints_scene,
    vector< DMatch >* good_matches) {
  good_matches->clear();
  if( !img_object.data || !img_scene.data )
  { std::cout<< " --(!) Error reading images " << std::endl; return; }

  double t = (double)getTickCount();

  // Parameter names can be looked up in
  // modules/features2d/src/features2d_init.cpp.
  Ptr<FeatureDetector> detector =
       Algorithm::create<FeatureDetector>("Feature2D.FAST");
  detector->set("threshold", 20);
  detector->set("nonmaxSuppression", true);

//  Ptr<FeatureDetector> detector =
//      cv::FeatureDetector::create("GridFAST");

//  Ptr<FeatureDetector> detector =
//      Algorithm::create<FeatureDetector>("Feature2D.BRISK");
  LOG(INFO) << "Done creating detector.";

  detector->detect(img_object, *keypoints_object);
  LOG(INFO) << "Done detecting img_object.";
  detector->detect(img_scene, *keypoints_scene);
  LOG(INFO) << "Done detecting img_scene.";
  LOG(INFO) << "#keypoints_object = " << keypoints_object->size();
  LOG(INFO) << "#keypoints_scene = " << keypoints_scene->size();
  t = ((double)getTickCount() - t)/getTickFrequency();
  cout << "Detection seconds: " << t << endl;

  cv::Mat descriptors_object, descriptors_scene;

  t = (double)getTickCount();

//  Ptr<Feature2D> extractor = Algorithm::create<Feature2D>("Feature2D.BRISK");
  //  int thresh = 60;
  //  int octaves = 4;
  //  float pattern_scales = 1.0f;

  Ptr<Feature2D> extractor = Algorithm::create<Feature2D>("Feature2D.ORB");

  extractor->compute(img_object, *keypoints_object, descriptors_object);
  extractor->compute(img_scene, *keypoints_scene, descriptors_scene);

  LOG(WARNING) << "#descriptors_object = " << descriptors_object.size();
  LOG(WARNING) << "#descriptors_scene = " << descriptors_scene.size();
  if (descriptors_object.empty() || descriptors_scene.empty()) {
    return;
  }

  t = ((double)getTickCount() - t)/getTickFrequency();
  cout << "Descriptor seconds: " << t << endl;

  //-- Step 3: Matching descriptor vectors using FLANN matcher
  t = (double)getTickCount();
//  Ptr<DescriptorMatcher> matcher =
//      Algorithm::create<DescriptorMatcher>("DescriptorMatcher.FlannBasedMatcher");
//  if(descriptors_object.type() != CV_32F) {
//    descriptors_object.convertTo(descriptors_object, CV_32F);
//  }
//
//  if(descriptors_scene.type() != CV_32F) {
//    descriptors_scene.convertTo(descriptors_scene, CV_32F);
//  }

  Ptr<DescriptorMatcher> matcher(new BFMatcher(NORM_HAMMING, true));

  vector< DMatch > matches;
  matcher->match( descriptors_object, descriptors_scene, matches);
//  vector<vector< DMatch > > match_groups;
//  matcher->radiusMatch( descriptors_object, descriptors_scene, match_groups, 10.f );
//  FlattenMatchGroups(match_groups, matches);
  LOG(INFO) << "matches->size() = " << matches.size();
  t = ((double)getTickCount() - t)/getTickFrequency();
  LOG(INFO) << "Match seconds: " << t;


  t = (double)getTickCount();
  if (matches.size() >= 4 && FLAGS_ransac_type != "none") {
    RansacTest(
        matches, *keypoints_object, *keypoints_scene, 2.0, 0.99, FLAGS_ransac_type == "Perspective",
        NULL, good_matches);
  } else {
    *good_matches = matches;
  }
  t = ((double)getTickCount() - t)/getTickFrequency();
  LOG(INFO) << "RANSAC seconds: " << t;
  LOG(INFO) << "good_matches->size() = " << good_matches->size();
}

void DrawMatchImage(
    const Mat& img_object, const Mat& img_scene, const std::string& fname) {
  vector<cv::KeyPoint> keypoints1;
  vector<cv::KeyPoint> keypoints2;
  vector< DMatch > matches;

  MatchImagePair(img_object, img_scene, &keypoints1, &keypoints2, &matches);
  LOG(INFO) << "#matches = " << matches.size();

  if (!FLAGS_output_fname.empty()) {
	  DumpMatches(keypoints1, keypoints2, matches, FLAGS_output_fname.c_str());
  }

  if (FLAGS_draw_matches) {
	  std::string ofname = fname;
	  size_t pos = ofname.find_last_of('/');
	  ofname = ofname.substr(pos + 1);
	  ofname = "/g/tmp/" + ofname + "_match.jpg";
	  LOG(INFO) << ofname;
	  WriteMatches(
	      img_object, img_scene, keypoints1, keypoints2, matches, ofname);
  }
}

/**
 * @function main
 * @brief Main function
 */
int main( int argc, char** argv ) {
  google::InitGoogleLogging(argv[0]);
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InstallFailureSignalHandler();
  FLAGS_logtostderr = true;

  if (FLAGS_task == "draw_matches") {
    Mat img_object = imread( argv[1], CV_LOAD_IMAGE_COLOR );
    Mat img_scene = imread( argv[1 + 1], CV_LOAD_IMAGE_COLOR );
    DrawMatchImage(img_object, img_scene, argv[1]);
  } else {
    LOG(FATAL) << "Unknown " << FLAGS_task;
  }

  return 0;
}
