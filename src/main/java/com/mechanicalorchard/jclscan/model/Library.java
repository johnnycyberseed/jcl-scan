package com.mechanicalorchard.jclscan.model;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SequencedCollection;

public class Library<T> {
  private Map<String,T> lib = new LinkedHashMap<>();

  public void register(String name, T obj) {
    lib.put(name, obj);
  } 

  public T resolve(String name) {
    return lib.get(name);
  }

  /* Return a set of objects registered in the library, maintaining insertion order */
  public SequencedCollection<T> registered() {
    return (SequencedCollection<T>) lib.values();
  }

  public int size() {
    return lib.entrySet().size();
  }
}
