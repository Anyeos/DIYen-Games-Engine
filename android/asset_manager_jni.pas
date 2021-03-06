{//from asset_manager_jni.h
/* Copyright (C) 2013 Anyeos
 * 
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
}

unit asset_manager_jni;

interface

uses asset_manager, jni;

{/**
 * Given a Dalvik AssetManager object, obtain the corresponding native AAssetManager
 * object.  Note that the caller is responsible for obtaining and holding a VM reference
 * to the jobject to prevent its being garbage collected while the native object is
 * in use.
 */}
//AAssetManager* AAssetManager_fromJava(JNIEnv* env, jobject assetManager);
function AAssetManager_fromJava(env: PJNIEnv; assetManager: jobject): PAAssetManager; cdecl; external 'libandroid.so';

implementation

end.
