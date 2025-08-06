package com.mechanicalorchard.jclscan.model;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AppSourceFile {
  private String name;
  private Kind kind;
  private Resource content;

  public AppSourceFile(String name, Kind kind, Resource content) {
    this.name = name;
    this.kind = kind;
    this.content = content;
  }

  public AppSourceFile(String name, Kind kind, String content) {
    this.name = name;
    this.kind = kind;
    this.content = new ByteArrayResource(content.getBytes());
  }

  public static enum Kind {
    ASSEMBLY,
    COBOL,
    EASYTRIEVE,
    JCL
  }
}
