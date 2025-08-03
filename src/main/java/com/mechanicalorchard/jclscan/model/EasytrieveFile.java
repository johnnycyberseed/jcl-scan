package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public final class EasytrieveFile implements Program {
  private String name;
  private String id;
  private Integer linesOfCode;
}
