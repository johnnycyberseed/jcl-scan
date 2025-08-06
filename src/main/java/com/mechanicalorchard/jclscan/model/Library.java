package com.mechanicalorchard.jclscan.model;

import java.util.HashMap;
import java.util.Map;

public class Library<T> {
  private Map<String,T> lib = new HashMap<>();

  public void register(String name, T obj) {
    lib.put(name, obj);
  } 
  public T resolve(String name) {
    return lib.get(name);
  }

  public int size() {
    return lib.entrySet().size();
  }
}
